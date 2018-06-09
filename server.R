shinyServer(function(input, output, session) {
  
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
  
  FPath <- NULL
  amUploadedRegData <- NULL
  RegDataTemp <- NULL
  rvData <- reactiveValues(RegData = NULL, CWGeog = NULL)
  amInputBuckets <- NULL
  amTransDataList <- NULL
  values <- reactiveValues()
  parametersDf <- NULL
  BucketValues <- reactiveValues()
  bucketData <- NULL
  modelCount <- reactiveValues(iterationCount = NULL)
  startDate <- NULL
  endDate <- NULL
  resultTable <- NULL
  modelScopeDf <- NULL
  finalDf <- NULL
  modelScopeDummyTable <- NULL
  resultTableDetail <- NULL
  allModelsResults <- NULL
  comparedResult <- NULL
  bucketVarData <- NULL
  filterModels <- NULL
  filteredModelResultsTable <- NULL
  topTable <- NULL
  
  
  ############################################################################################
  ############ ACQUIRE BEGINS ################################################################
  ############################################################################################
  
  output$upload <- renderUI({
    tagList(
      fileInput('file1','Upload file if you are not using variable console',accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
      fluidRow(
        column(2,actionButton("uploadData","Continue")),
        column(1,h4("OR")),
        column(4,actionButton("inputVOFFile","Import from Variable Console")),
        column(4,downloadButton("downLoadSampleAF", "Download Sample AF File"))
      )
    )
  })
  
  output$downLoadSampleAF <- downloadHandler(
    filename = function() {
      paste("AutoModeller_Sample_Dataset", "csv", sep = ".")
    },
    content = function(file) {
      load(paste0(getwd(),"/AutoModeller_Sample_Dataset.RData"))
      write.csv(x = AutoModeller_Sample_Dataset,file = file,row.names = F,quote = FALSE)
      rm(AutoModeller_Sample_Dataset)
    }
  )
  
  automodeller <- reactiveValues(bucket = NULL)
  automodeller$bucket <- 0
  
  observeEvent(input$uploadData, {
    if (is.null(input$file1)){
      return(NULL)
    }else {
      withProgress(message = 'Uploading Data....', value = 0.85, {
        inFile <- input$file1
        amUploadedRegData <<-read.csv(inFile$datapath,header = TRUE,stringsAsFactors = FALSE)
        amInputBuckets <<- unique(as.character(amUploadedRegData[1,]))[-1]
      })
      
      if(!any(names(amUploadedRegData) %in% "Period") | !any(as.character(amUploadedRegData[1,]) %in% "Dependent")){
        amUploadedRegData <<- NULL
        shinyjs::reset("file1")
        meCustomAlert(message = "Period Column or Dependent Bucket is not present. It should be mentioned 'Period'as column and 'Dependent' as Bucket. Please refresh the page before loading data again.",alertType = "error")
      }else if(any(is.na(apply(amUploadedRegData[-1,-1],2,as.numeric)))){
        amUploadedRegData <<- NULL
        shinyjs::reset("file1")
        meCustomAlert(message = "Data should be in numeric format other than Bucket row. Please refresh the page before loading data again.",alertType = "error")
      }else if(sum(amUploadedRegData[1,] == "Dependent",na.rm = T)!= 1){
        amUploadedRegData <<- NULL
        shinyjs::reset("file1")
        meCustomAlert(message = "Bucket Issue!, There should be only one 'Dependent' in the uploaded data.",alertType = "error")
      }else {
        vars <- names(amUploadedRegData)[-which(names(amUploadedRegData) %in% "Period")]
        observe({input$uploadData
          isolate({
            automodeller$bucket <- amInputBuckets[-which(amInputBuckets %in% "Dependent")]
          })
        })
        displayVarSelectUI(vars)
      }
    }
  })
  
  observeEvent(input$inputVOFFile, {
    if (is.null(MasterDF)){
      meCustomAlert(message = "No Data Available at Variable Console!, Please provide data from Variable Console.",alertType = "error")
    }else{
      withProgress(message = 'Uploading Data....', value = 0.85, {
        amUploadedRegData <<-as.data.frame(variableConsoleFile(MasterDF, "outputFile", varTag))
        amInputBuckets <<- unique(as.character(amUploadedRegData[1,]))[-1]
      })
      vars <- names(amUploadedRegData)[-which(names(amUploadedRegData) %in% "Period")]
      observe({input$inputVOFFile
        isolate({
          automodeller$bucket <- amInputBuckets[-which(amInputBuckets %in% "Dependent")]
        })
      })
      displayVarSelectUI(vars)
    }
  })
  
  displayVarSelectUI <- function(vars){
    output$displayVarSelect <- renderUI({
      tagList(
        box(title = "Select Variables & Transformation for Modelling",status = "info",width = "12",solidHeader = T,collapsible = F,
            selectizeInput("selectedVars", "Select variables for Modelling:", choices  = vars, selected = NULL, multiple = TRUE,options = list(plugins = list('remove_button', 'drag_drop'))),
            radioButtons(inputId = "getDecayPos",label = "Select Decay Type",choices = c("Decay First", "Decay Last"),selected = NULL, inline = TRUE),
            fluidRow(
              column(1, actionButton("transButton", "Continue",style="color: #212121; background-color: #b3e5fc; border-color: #2e6da4")),
              column(4, downloadButton("downLoadCorDF", "Download Correlation Matrix",style="color: #212121; background-color: #b3e5fc; border-color: #2e6da4"))
            )
        ),
        uiOutput("transPop")
      )
    })
  }
  
  output$downLoadCorDF <- downloadHandler(
    filename = function() {
      paste("Correlation_Matrix", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(x = as.data.frame(cor(apply(amUploadedRegData[-1,-1],2, as.numeric))),file = file,row.names = T,quote = FALSE)
    }
  )
  
  output$transPop <- renderUI({
    observe({input$transButton
      isolate({automodeller$bucket})
    })
    if(input$getDecayPos== "Decay First"){
      tagList(
        bsModal(id = "getTransRange", title = "Transformation Range", trigger = "transButton",
                box(status = "info",solidHeader = TRUE,title = "Provide Transformation Value..",width = 12,
                    fluidRow(
                      column(12,selectizeInput('selectedTransBucket', "Select Buckets for Transformation", choices = automodeller$bucket,multiple = TRUE, selected = NULL, options = list(plugins = list('remove_button', 'drag_drop'))))
                    ),
                    fluidRow(
                      column(12,selectizeInput('decaySelection', "Select Transformation", choices = c("S-Curve"="Decay Alpha","Power"="Decay Power"),options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
                      ))
                    ),
                    fluidRow(
                      uiOutput("transSelectionInput")
                    ),
                    fluidRow(
                      column(12,actionButton("continueFileSelection","Continue",style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:right"))
                    )
                )
        )
      )
    }else
      if(input$getDecayPos== "Decay Last"){
        tagList(
          bsModal(id = "getTransRange", title = "Transformation Range", trigger = "transButton",
                  tagList(
                    box(status = "info",solidHeader = TRUE,title = "Provide Transformation Value..",width = 12,
                        fluidRow(
                          column(12,selectizeInput('selectedTransBucket', "Select Buckets for Transformation", choices = automodeller$bucket,multiple = TRUE, selected = NULL, options = list(plugins = list('remove_button', 'drag_drop'))))
                        ),
                        fluidRow(
                          column(12,selectizeInput('decaySelection', "Select Transformation", choices = c("S-Curve"="Alpha Decay","Power"="Power Decay"),options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
                          ))
                        ),
                        fluidRow(
                          uiOutput("transSelectionInput")
                        ),
                        fluidRow(
                          column(12,actionButton("continueFileSelection","Continue",style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:right"))
                        )
                    )
                  )
          )
        )
      }
  })
  
  output$transSelectionInput <- renderUI({
    decaySelection <- input$decaySelection
    
    displayTransformationOption <- function(transType){
      if(transType == "lag"){
        tagList(
          column(2,"  "),
          column(4,numericInput('getLagMin', "Lag Min", value = 0, min = 0, max = 5,step =1)),
          column(4,numericInput('getLagMax', "Lag Max", value = 1, min = 0, max = 5,step =1))
          #column(4,numericInput('getLagSteps', "Lag Steps", value = 1, min = 1, max = 1,step =1))
        )
      }else if(transType == "alpha"){
        tagList(
          column(4,numericInput('getAlphaMin', "Alpha Min", value = 0.7, min = 0.01, max = 0.99,step =0.01)),
          column(4,numericInput('getAlphaMax', "Alpha Max", value = 0.8, min = 0.01, max = 0.99,step =0.01)),
          column(4,numericInput('getAlphaSteps', "Alpha Steps",  value = 0.05, min = 0.01, max = 0.1,step =0.01))
        )
      }else if(transType == "power"){
        tagList(
          column(4,numericInput('getPowerMin', "Power Min", value = 0.4, min = 0.1, max = 0.9,step =0.1)),
          column(4,numericInput('getPowerMax', "Power Max", value = 0.6, min = 0.1, max = 0.9,step =0.1)),
          column(4,numericInput('getPowerSteps', "Power Steps", value = 0.1, min = 0.1, max = 0.8,step =0.1))
        )
      }else if(transType == "decay"){
        tagList(
          column(4,numericInput('getDecayMin', "Decay Min", value = 0.45, min = 0.01, max = 0.99,step =0.01)),
          column(4,numericInput('getDecayMax', "Decay Max", value = 0.55, min = 0.01, max = 0.99,step =0.01)),
          column(4,numericInput('getDecaySteps', "Decay Steps", value = 0.05, min = 0.01, max = 0.1,step =0.01))
        )
      }
    }
    
    if(decaySelection == "Alpha Decay"){
      box(width = 12,fluidRow(displayTransformationOption(transType = "lag")),
          fluidRow(displayTransformationOption(transType = "alpha")),
          fluidRow(displayTransformationOption(transType = "decay"))
      ) 
    } else if(decaySelection == "Power Decay"){
      box(width = 12,fluidRow(displayTransformationOption(transType = "lag")),
          fluidRow(displayTransformationOption(transType = "power")),
          fluidRow(displayTransformationOption(transType = "decay"))
      ) 
    }else if(decaySelection == "Decay Power"){
      box(width = 12,fluidRow(displayTransformationOption(transType = "lag")),
          fluidRow(displayTransformationOption(transType = "decay")),
          fluidRow(displayTransformationOption(transType = "power"))
      ) 
    }else if(decaySelection == "Decay Alpha"){
      box(width = 12,fluidRow(displayTransformationOption(transType = "lag")),
          fluidRow(displayTransformationOption(transType = "decay")),
          fluidRow(displayTransformationOption(transType = "alpha"))
      ) 
    }
    
  })
  
  observeEvent(input$continueFileSelection,{
    # need to check for bucket and transformation selection.
    if(input$decaySelection=="" | is.null(input$selectedTransBucket)){
      meCustomAlert(message = "Transformation criteria is incomplete.",alertType = "warning")
    }else{
      toggleModal(session, "getTransRange", toggle = "close")
      
      withProgress(message = 'File Uploaded. Transforming Variables...', value = 0.85, {
        RegDataTemp <<- amUploadedRegData[,which(names(amUploadedRegData) %in% c("Period",input$selectedVars))]
        amInputBuckets <<- data.table(variableName = rownames(t(RegDataTemp[1, ])),bucket = as.character(t(RegDataTemp[1, ])),stringsAsFactors = F)[-1, ]
        amInputBucketList <- split(amInputBuckets$variableName, amInputBuckets$bucket)
        
        transBucketParameter <- list(input$selectedTransBucket,input$decaySelection,input$getLagMin, input$getLagMax, input$getDecayMin, input$getDecayMax, input$getDecaySteps, input$getPowerMin, input$getPowerMax, input$getPowerSteps, input$getAlphaMin, input$getAlphaMax, input$getAlphaSteps)
        names(transBucketParameter) <- c("selectedTransBucket","decaySelection","getLagMin", "getLagMax", "getDecayMin", "getDecayMax", "getDecaySteps", "getPowerMin", "getPowerMax", "getPowerSteps", "getAlphaMin", "getAlphaMax", "getAlphaSteps")     
        
        system.time(amTransDataList <- CreateAllTransformations(RegDataTemp[-1, ], transBucketParameter, amInputBucketList))
        
        finalList <- list()
        #Mappping transformation with buckets
        for (name in names(amInputBucketList)) {
          finalList[[name]] <- amTransDataList[which(names(amTransDataList) %in% amInputBucketList[[name]])]
        }
        amTransDataList <<- finalList
      })
      
      output$contents <- renderDataTable(datatable({
        RegDataTemp[-1, ]
      }, rownames = FALSE, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),paging = TRUE,scrollX = TRUE,
        searching = FALSE,autoWidth = TRUE,columnDefs = list(list(width = '120px', targets = "_all"))), selection = 'none')
      )
      
      output$amData <- renderUI({
        box(title = "Actual Data",status = "success",width = "12",solidHeader = T,collapsible = F,
            DT::dataTableOutput('contents'),
            fluidRow(
              column(12, actionButton("navToDataReview","Define Model Manager", style="color: #212121; background-color: #98fbc9; border-color: #212121; float: left"))
            )
        )
      })
    }
  })
  
  observeEvent(input$navToDataReview,{
    updateTabItems(session, "sidebartabs", "AM_Data_Review")
  })
  
  
  ############################################################################################
  ################################### ACQUIRE ENDS ###########################################
  ############################################################################################
  
  ############################################################################################
  ################################### AUTOMODELLER MODULE BEGINS #############################
  ############################################################################################
  
  ############################################################################################
  ################################### AUTOMODELLER DATA REVIEW TAB START #####################
  ############################################################################################
  
  data = reactive({
    if (!is.null(input$AMExplore)) {
      DF <- hot_to_r(input$AMExplore)
    } else {
      if (is.null(values[["DF"]])){
        if(input$decaySelection == "Alpha Decay"){
          DF = data.frame(
            VariableName = colnames(RegDataTemp)[-1],
            
            Bucket = factor(as.character(RegDataTemp[1, ][-1]), levels = unique(as.character(RegDataTemp[1, ][-1]))),
            
            ModellingFlag = factor(rep("Yes", times = length(RegDataTemp) - 1),levels = c("Yes", "No")),
            
            #Variable transformation
            Transformation = factor(rep("Linear", times = length(RegDataTemp) - 1),levels = c("Linear", "Alpha Decay")),
            
            #Lag minimum
            LagMin = factor(rep(input$getLagMin, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
            
            #Lag maximum
            LagMax = factor(rep(input$getLagMax, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
            
            #alpha minimum
            AlphaMin = factor(rep(input$getAlphaMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getAlphaMin,to = input$getAlphaMax,
                                                                                                   by = input$getAlphaSteps)),
            
            #alpha maximum
            AlphaMax = factor(rep(input$getAlphaMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getAlphaMin,to = input$getAlphaMax,
                                                                                                   by = input$getAlphaSteps)),
            
            #alpha steps
            AlphaSteps = factor(rep(input$getAlphaSteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.1,by = 0.01)),
            
            #decay minimum
            DecayMin = factor(rep(input$getDecayMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                   by = input$getDecaySteps)),
            
            #decay maximum
            DecayMax = factor(rep(input$getDecayMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                   by = input$getDecaySteps)),
            
            #decay steps
            DecaySteps = factor(rep(input$getDecaySteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.10,by = 0.01)),
            
            #T stat Direction
            TstatDir = rep(as.numeric(0), times = length(RegDataTemp) - 1),
            
            stringsAsFactors = F
          )
        } else 
          if(input$decaySelection == "Decay Alpha"){
            DF = data.frame(
              VariableName = colnames(RegDataTemp)[-1],
              
              Bucket = factor(as.character(RegDataTemp[1, ][-1]), levels = unique(as.character(RegDataTemp[1, ][-1]))),
              
              ModellingFlag = factor(rep("Yes", times = length(RegDataTemp) - 1),levels = c("Yes", "No")),
              
              #Variable transformation
              Transformation = factor(rep("Linear", times = length(RegDataTemp) - 1),levels = c("Linear", "Decay Alpha")),
              
              #Lag minimum
              LagMin = factor(rep(input$getLagMin, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
              
              #Lag maximum
              LagMax = factor(rep(input$getLagMax, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
              
              #decay minimum
              DecayMin = factor(rep(input$getDecayMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                     by = input$getDecaySteps)),
              
              #decay maximum
              DecayMax = factor(rep(input$getDecayMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                     by = input$getDecaySteps)),
              
              #decay steps
              DecaySteps = factor(rep(input$getDecaySteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.10,by = 0.01)),
              
              #alpha minimum
              AlphaMin = factor(rep(input$getAlphaMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getAlphaMin,to = input$getAlphaMax,
                                                                                                     by = input$getAlphaSteps)),
              
              #alpha maximum
              AlphaMax = factor(rep(input$getAlphaMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getAlphaMin,to = input$getAlphaMax,
                                                                                                     by = input$getAlphaSteps)),
              
              #alpha steps
              AlphaSteps = factor(rep(input$getAlphaSteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.1,by = 0.01)),
              
              #T stat Direction
              TstatDir = rep(as.numeric(0), times = length(RegDataTemp) - 1),
              
              stringsAsFactors = F
            )
          } else 
            if(input$decaySelection == "Decay Power"){
              DF = data.frame(
                VariableName = colnames(RegDataTemp)[-1],
                
                Bucket = factor(as.character(RegDataTemp[1, ][-1]), levels = unique(as.character(RegDataTemp[1, ][-1]))),
                
                ModellingFlag = factor(rep("Yes", times = length(RegDataTemp) - 1),levels = c("Yes", "No")),
                
                #Variable transformation
                Transformation = factor(rep("Linear", times = length(RegDataTemp) - 1),levels = c("Linear", "Decay Power")),
                
                #Lag minimum
                LagMin = factor(rep(input$getLagMin, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
                
                #Lag maximum
                LagMax = factor(rep(input$getLagMax, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
                
                #decay minimum
                DecayMin = factor(rep(input$getDecayMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                       by = input$getDecaySteps)),
                
                #decay maximum
                DecayMax = factor(rep(input$getDecayMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                       by = input$getDecaySteps)),
                
                #decay steps
                DecaySteps = factor(rep(input$getDecaySteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.10,by = 0.01)),
                
                #Power minimum
                PowerMin = factor(rep(input$getPowerMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getPowerMin,to = input$getPowerMax,
                                                                                                       by = input$getPowerSteps)),
                
                #Power maximum
                PowerMax = factor(rep(input$getPowerMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getPowerMin,to = input$getPowerMax,
                                                                                                       by = input$getPowerSteps)),
                
                #Power steps
                PowerSteps = factor(rep(input$getPowerSteps, times = length(RegDataTemp) -1), seq(from = 0.1,to = 0.9,by = 0.1)),
                
                #T stat Direction
                TstatDir = rep(as.numeric(0), times = length(RegDataTemp) - 1),
                
                stringsAsFactors = F
              )
            } else 
              if(input$decaySelection == "Power Decay"){
                DF = data.frame(
                  VariableName = colnames(RegDataTemp)[-1],
                  
                  Bucket = factor(as.character(RegDataTemp[1, ][-1]), levels = unique(as.character(RegDataTemp[1, ][-1]))),
                  
                  ModellingFlag = factor(rep("Yes", times = length(RegDataTemp) - 1),levels = c("Yes", "No")),
                  
                  #Variable transformation
                  Transformation = factor(rep("Linear", times = length(RegDataTemp) - 1),levels = c("Linear", "Power Decay")),
                  
                  #Lag minimum
                  LagMin = factor(rep(input$getLagMin, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
                  
                  #Lag maximum
                  LagMax = factor(rep(input$getLagMax, times = length(RegDataTemp) -1), levels = input$getLagMin:input$getLagMax),
                  
                  #Power minimum
                  PowerMin = factor(rep(input$getPowerMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getPowerMin,to = input$getPowerMax,
                                                                                                         by = input$getPowerSteps)),
                  
                  #Power maximum
                  PowerMax = factor(rep(input$getPowerMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getPowerMin,to = input$getPowerMax,
                                                                                                         by = input$getPowerSteps)),
                  
                  #Power steps
                  PowerSteps = factor(rep(input$getPowerSteps, times = length(RegDataTemp) -1), seq(from = 0.1,to = 0.9,by = 0.1)),
                  
                  #decay minimum
                  DecayMin = factor(rep(input$getDecayMin, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                         by = input$getDecaySteps)),
                  
                  #decay maximum
                  DecayMax = factor(rep(input$getDecayMax, times = length(RegDataTemp) - 1),levels = seq(from = input$getDecayMin,to = input$getDecayMax,
                                                                                                         by = input$getDecaySteps)),
                  
                  #decay steps
                  DecaySteps = factor(rep(input$getDecaySteps, times = length(RegDataTemp) - 1),levels = seq(from = 0.01,to = 0.10,by = 0.01)),
                  
                  #T stat Direction
                  TstatDir = rep(as.numeric(0), times = length(RegDataTemp) - 1),
                  
                  stringsAsFactors = F
                )
              }
      }
      else{
        DF = values[["DF"]]
      }
    }
    values[["DF"]] = DF
    DF
  })
  
  # Ashutosh: Changing in AMExplore table display at
  
  output$displayModelManager <- renderUI({
    tagList(
      fluidRow(
        box(title = "Model Manager (Modelling Rule)",status = "success",width = "12",solidHeader = T,collapsible = F,
            rHandsontableOutput("AMExplore"),
            fluidRow(
              column(2, actionButton("parametersData","Update Parameters",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4")),
              column(4, actionButton("backCsvUpload","Back",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4"))
            )
        ),
        uiOutput("displayBucketData"),
        uiOutput("displayModelScope")
      )
    )
  })
  
  output$AMExplore <- renderRHandsontable({
    parametersDf <<- data()
    if (!is.null(parametersDf))
      rhandsontable(parametersDf, useTypes = TRUE, stretchH = "all", selectCallback = T,
                    fillHandle = list(direction='vertical', autoInsertRow=FALSE), 
                    maxRows = nrow(parametersDf)) %>%
      hot_col(c("VariableName","Bucket"), readOnly = TRUE)%>%
      hot_rows(fixedRowsTop = 1) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>% 
      hot_cols(fixedColumnsLeft = 1, renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col == 0 | col == 1) {
                  td.style.background = '#fff3e0';
                 }else if(instance.getData()[row][1] == 'Dependent'){
                  td.style.background = '#88f4e9';
                 }else if (instance.getData()[2] == 'Yes') {
                  td.style.background = '#ffede9';
                 }else if (instance.getData()[row][2] == 'No') {
                  td.style.background = '#dddddd';
                 }else if(instance.getData()[row][3] == 'Power Decay' | instance.getData()[row][3] == 'Alpha Decay'| instance.getData()[row][3] == 'Decay Power' | instance.getData()[row][3] == 'Decay Alpha'){
                  td.style.background = '#dcedc8';
                 }else if(instance.getData()[row][3] == 'Linear'){
                  td.style.background = '#ffccbc';
                 }
              }"
      ) 
  })
  
  observeEvent(input$backCsvUpload,{
    updateTabItems(session, "sidebartabs", "directCsvUpload")
  })
  
  mmModelFlagCount <- NULL
  
  bucketDataReactive <- reactive({
    if (!is.null(input$bucketInput)) {
      bucketData = hot_to_r(input$bucketInput)
      if(mmModelFlagCount != sum(parametersDf$ModellingFlag=="Yes")){
        bucketData = data.frame(Bucket = factor(unique(as.character(RegDataTemp[1, ][-1])), levels = unique(as.character(RegDataTemp[1, ][-1]))),
                                MinVariables = factor(rep(0, times = length(unique(as.character(RegDataTemp[1, ][-1])))), levels = 0:10),
                                MaxVariables = factor(rep(1, times = length(unique(as.character(RegDataTemp[1, ][-1])))), levels = 1:10),
                                stringsAsFactors = F
        )
        mmModelFlagCount <<- sum(parametersDf$ModellingFlag=="Yes")
      }
      
    } else {
      if (is.null(BucketValues[["bucketData"]])){
        bucketData = data.frame(Bucket = factor(unique(as.character(RegDataTemp[1, ][-1])), levels = unique(as.character(RegDataTemp[1, ][-1]))),
                                MinVariables = factor(rep(0, times = length(unique(as.character(RegDataTemp[1, ][-1])))), levels = 0:10),
                                MaxVariables = factor(rep(1, times = length(unique(as.character(RegDataTemp[1, ][-1])))), levels = 1:10),
                                stringsAsFactors = F
        )
        mmModelFlagCount <<- sum(parametersDf$ModellingFlag=="Yes")
      }
      else
        bucketData = BucketValues[["bucketData"]]
    }
    BucketValues[["bucketData"]] = bucketData
    bucketData
  })
  
  observeEvent(input$parametersData, {
    parametersDf <<- isolate(values[["DF"]])
    if(!all(parametersDf$Bucket[which(parametersDf$Transformation != "Linear")] %in% input$selectedTransBucket)){
      meCustomAlert(message = "Wrong transformation selection!, Non Linear Transformation should be only selected for the transform bucket.",alertType = "error")
    }else{
      bucketTransType <- parametersDf[which(parametersDf$Bucket %in% input$selectedTransBucket & parametersDf$ModellingFlag == "Yes"),c("Bucket","Transformation")]
      bucketTransfactor <- NULL
      for (bucket in input$selectedTransBucket) {
        bucketTransfactor <-  c(bucketTransfactor,length(unique(bucketTransType[which(bucketTransType$Bucket == bucket),2])))
      }
      
      if(!all(bucketTransfactor == 1)){
        meCustomAlert(message = "Invalid Transformation selection!, Please select same transformation for each Bucket where the variable has modellingFlag as 'Yes'.",alertType = "error")
      }else{
        for (bucket in input$selectedTransBucket) {
          parametersDf[which(parametersDf$Bucket %in% bucket & parametersDf$ModellingFlag == "No"),"Transformation"] <<- unique(parametersDf[which(parametersDf$Bucket %in% bucket & parametersDf$ModellingFlag == "Yes"),c("Transformation")])
        }
        output$displayBucketData <- renderUI({
          box(title = "Number of Variables in model by bucket",status = "success",width = "6",solidHeader = T,collapsible = F,
              rHandsontableOutput("bucketInput"),
              actionButton("bucketsData","Update bucket data",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4")
          )
        })
        
        # Ashutosh: Changing in bucket table display at UI.
        output$bucketInput <- renderRHandsontable({
          bucket <- unique(as.character(parametersDf[parametersDf$ModellingFlag == "Yes",2]))
          bucketData <<- bucketDataReactive()
          if (!is.null(bucketData)) {
            rhandsontable(data.frame(bucketData[bucketData$Bucket %in% bucket & !bucketData$Bucket=="Dependent",],row.names = NULL), 
                          useTypes = TRUE, stretchH = "all",
                          fillHandle = FALSE) %>%
              hot_col("Bucket", readOnly = TRUE)%>%
              hot_cols(fixedColumnsLeft = 1) %>%
              hot_rows(fixedRowsTop = 1) %>%
              hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
          }
        })
      }
    }
  })
  
  observeEvent(input$bucketsData, {
    bucketData <<- isolate(BucketValues[["bucketData"]])
    parametersDfTmp <- parametersDf[parametersDf$ModellingFlag == "Yes",1:2]
    bucketList <- split(parametersDfTmp$VariableName,factor(parametersDfTmp$Bucket))
    bucketList[["Dependent"]] <- NULL
    bucketSize <- as.numeric(unlist(lapply(bucketList, function(x) length(x))))
    
    if(all(as.numeric(bucketData$MaxVariables) <= bucketSize)){
      output$displayModelScope <- renderUI({
        box(title = "Select Modelling scope",status = "warning",width = "6",solidHeader = T,
            uiOutput('modellingScope')
        )
      })
      
      Period <- lubridate::dmy(RegDataTemp[-1,1])
      # capturing the user input date for model scope which will be persistent untill user change again.
      
      if(is.null(input$startDate) & is.null(input$endDate)){
        start_date <- min(Period)
        end_date <- max(Period)
      }else if(input$startDate == min(Period) & input$endDate == max(Period)){
        start_date <- min(Period)
        end_date <- max(Period)
      }else {
        start_date <- input$startDate
        end_date <- input$endDate
      }
      
      
      output$modellingScope <- renderUI({
        tagList(
          dateInput('startDate', label = "Modelling start Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = start_date,min = min(Period),max = max(Period)),
          dateInput('endDate', label = "Modelling end Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = end_date,min = min(Period),max = max(Period)),
          actionButton("modelScope", "Apply",style="color: #212121; background-color: #fbc998; border-color: #2e6da4")
        )
      })
    }else{
      meCustomAlert(message = "Bucket values is out of range. Check the number of variables in each buckets.",alertType = "error")
    }
    
  })
  
  #--------------------------------------------------------------------------
  # Ashutosh: have updated date validation format, now select modelling scope will accept only dd-mm-yyyy format of date in start and end date input.
  #--------------------------------------------------------------------------
  
  ################################### AM Regression ###########################################
  
  observeEvent(input$modelScope, {
    if(length(input$startDate)== 0 | length(input$endDate)== 0){
      meCustomAlert(message = "Date Format Error!, Please check the date.",alertType = "error")
    }else {
      startDate <<- input$startDate
      endDate <<- input$endDate
      updateTabsetPanel(session, "sidebartabs", selected = "AM_Result")
    }
  })
  
  observeEvent(input$backAMDataReview,{
    updateTabItems(session, "sidebartabs", "AM_Data_Review")
  })
  
  output$displayModelResult <- renderUI({
    tagList(
      fluidRow(
        box(title = "Result Output",status = "success",width = "12",solidHeader = T,collapsible = F,
            wellPanel(
              fluidRow(
                column(2, actionButton("refreshModels","Run Regression",icon = icon("refresh"),style="color: #212121; background-color: #98fbc9; border-color: #212121;float:right")),
                column(9,uiOutput("amModelSort"))
              )
            ),
            br(),
            fluidRow(
              column(12, dataTableOutput("resultTable"))
            ),
            br(),
            wellPanel(
              fluidRow(
                column(1, actionButton("backAMDataReview","Back",style="color: #212121; background-color: #98fbc9; border-color: #212121;float:right")),
                column(11,uiOutput("amModelOption"))
              )
            ),
            bsModal(id = "itrCountModel", title = "Iteration Count", trigger = "refreshModels",
                    tagList(
                      uiOutput("itrBox")
                    )
            )
        )
      ),
      uiOutput("modelDetails")
    )
  })
  
  modelCount$iterationCount <- 0
  baseFormula <- NULL
  
  output$itrBox <- renderUI({
    
    displayItrCountUI <- function(){
      tagList(
        textOutput("itrCountText"),
        br(),
        selectizeInput("rankType","Select Ranking Algorithm to Rank Models",choices = c("Nimish Algorithm" = "Ranking1", "Sounava Algorithm" = "Ranking2"),options = list(placeholder = 'Please select a Ranking Algo', onInitialize = I('function() { this.setValue(""); }'))),
        br(),
        actionButton("runModels","Continue", style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:left"),
        actionButton("itrCountModelClose","Modify Parameter", style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:left")
      )
    }
    withProgress(message = 'Calculating iteration count.', detail = 'This may take a while..., please be patient.', value = 0.85, {
      print("Formula Building Started...")
      print(paste0("Started...@ ", Sys.time()))
      st <- Sys.time()
      observe({input$refreshModels
        baseFormula <<- getIterCount(RegDataTemp,amTransDataList,bucketData,parametersDf, startDate, endDate)
        isolate({
          modelCount$iterationCount <- length(baseFormula)
        })
      })
      et <- Sys.time()
      print(et-st)
      print(paste0("Ended...@ ", Sys.time()))
      if(as.numeric(modelCount$iterationCount) < 5000){
        fluidRow(box(status = "success",solidHeader = TRUE,title = "Optimal Number of Iterations. Please proceed..",width = 12,displayItrCountUI()
        )
        )
      } else
        if(as.numeric(modelCount$iterationCount) < 20000){
          fluidRow(box(status = "warning",solidHeader = TRUE,title = "Iterations above optimal cutoff. Please reconsider parameters..",width = 12,
                       displayItrCountUI()
          )
          )
        } else 
          if(as.numeric(modelCount$iterationCount) < 200000) {
            fluidRow(box(status = "danger",solidHeader = TRUE, title = "Iterations number is too high. Tool will be consume lots of internal memory.", width = 12,
                         displayItrCountUI()
            )
            )
          }else 
            if(as.numeric(modelCount$iterationCount) >= 200000) {
              fluidRow(
                box(status = "danger",solidHeader = TRUE, title = "Iterations number is too high. System may crash.", width = 12,
                    itrCount <- paste("With the given parameters, ModellerEngine will try to generate more than 200,000 Models which is too high, please reduce the variable combinations"),
                    br(),
                    actionButton("itrCountModelClose","Modify Parameter", style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:left")
                )
              )
            }
    })
  })
  
  output$itrCountText <- renderText({
    withProgress(
      message = 'Calculating iteration count.', detail = 'This may take a while..., please be patient.', value = 0.85, {
        observe({input$refreshModels
          isolate({modelCount$iterationCount})
        })
        itrCount <- paste("With the given parameters, ModellerEngine will generate",modelCount$iterationCount," Models. Which will take approximately ",ceil((as.numeric(modelCount$iterationCount)*0.05)/60)," minutes. The Final model count may vary in case Tstat direction is provided for the variable.")
      })
  })
  
  observeEvent(input$itrCountModelClose, {
    toggleModal(session, "itrCountModel", toggle = "close")
    updateTabItems(session, "sidebartabs", "AM_Data_Review")
  })
  
  observeEvent(input$runModels, {
    toggleModal(session, "itrCountModel", toggle = "close")
    tryCatch({
      withProgress(
        message = 'Modelling is inprogress.', detail = 'This may take a while..., please be patient.', value = 0.85, {
          print(paste0("Modelling Started...@ ", Sys.time()))
          st <- Sys.time()
          resultData <- getDataForTransformation(RegDataTemp,amTransDataList, parametersDf, bucketData,startDate, endDate, baseFormula, input$rankType)
          et <- Sys.time()
          print(et-st)
          print(paste0("Modelling ended...@ ", Sys.time()))
          resultTable <<- resultData[["Models"]]
          modelScopeDf <<- resultData[["ModelScopeDf"]]
          finalDf <<-  resultData[["finalDf"]]
          allModelsResults <<- resultData[["modelList"]]
          modelScopeDummyTable <<- NULL
          # allModelList will have total number of dummy model generated through main model.
          allModelsList <<- as.list(rep(0,length(allModelsResults)))
          displayResultTable(resultTable)
        }
      )
    }, error = function(err){
      meCustomAlert(message = "Please check the Dataset, Model Manager, Bucket Selection or Date range issue.",alertType = "error")
    })
    
    output$amModelSort <- renderUI({
      fluidRow(
        column(4, selectizeInput('sortVar', "Select Order of Columns for Sorting", choices = c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE","Model_Score"),multiple = TRUE,options = list(placeholder = 'Please select columns', onInitialize = I('function() { this.setValue(""); }')))),
        column(4, actionButton("sortResult","Sort Result",icon = icon("refresh"),style="color: #212121; background-color: #98fbc9; border-color: #212121;float:left")),
        column(2, actionButton("amModelRefresh","Refresh",icon = icon("refresh"),style="color: #212121; background-color: #98fbc9; border-color: #212121;float:left"))
      )
    })
    
    output$amModelOption <- renderUI({
      fluidRow(
        column(2, downloadButton("downLoadResults", "Download Model",style="color: #212121; background-color: #98fbc9; border-color: #212121; float:left")),
        column(2, downloadButton("downLoadData", "Download Data",style="color: #212121; background-color: #98fbc9; border-color: #212121; float:left")),
        column(2, actionButton("compareModel","Compare Model",style="color: #212121; background-color: #98fbc9; border-color: #212121; float:left"))
      )
    })
    
  })
  
  observeEvent(input$sortResult,{
    if(length(resultTable)==7 | length(resultTable) == 1| is.null(resultTable) | is.null(input$sortVar)){
      meCustomAlert(message = "OOps, Either Sort variable is not selected or Try sorting for Multiple Models",alertType = "warning")
    }else if(length(input$sortVar)==1){
      sortColumn <- input$sortVar
      displayResultTable(resultTable[order(resultTable[,match(sortColumn,names(resultTable))],decreasing = TRUE),])
    }else if(length(input$sortVar) >1){
      sortColumn <- input$sortVar
      displayResultTable(resultTable[do.call( order , c(resultTable[ , match( sortColumn , names(resultTable))],decreasing = TRUE)),])  
    }
  })
  
  displayResultTable <- function(resultTable){
    resultTableDetail <<- resultTable
    
    if(length(resultTable) == 1){
      meCustomAlert(message = "No Model Shortlisted!, Given T-Stat value is more than available T-Stat value for particular variable/variables in the model. Please check the T-Stat value.",alertType = "warning")
    }else{
      output$resultTable <- renderDataTable(
        datatable({resultTable[,-1]
        }, rownames = FALSE, extensions = 'Buttons', 
        options = list(
          dom = 'Bfrtip',autoWidth = TRUE,columnDefs = list(list(width = "120px", targets = "_all")),
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = TRUE,scrollX = TRUE,scrollY = TRUE,searching = FALSE
        ), selection = list(mode = "multiple"), height = "2000px") %>%
          formatRound(columns=c(4,5,6,7,8,9), digits=5),server = TRUE)
    }
  }
  
  baseModelParameter <- reactive({
    i  <- input$resultTable_rows_selected
    s <- resultTableDetail[i,1]
    datatable({
      if (length(s)) {
        resultDetail <- getElasticity(allModelsResults[[s]],parametersDf = parametersDf)
      }else {
        resultDetail <- getElasticity(allModelsResults[[1]],parametersDf = parametersDf)
      }
      resultDetail[,2:9] <- sapply(resultDetail[,2:9], function(x) round(x, digits = 5))
      resultDetail
    },rownames = FALSE, extensions = 'Buttons' ,options = list(
      dom = 'Bfrtip',autoWidth = TRUE,columnDefs = list(list(width = "120px", targets = "_all")),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      paging = FALSE,scrollX = TRUE,scrollY = TRUE,searching = FALSE
    ), selection = 'none') 
  })
  
  output$parameters = renderDataTable({
    baseModelParameter()
  })
  
  baseModelAVMPlot <- reactive({
    i  <- input$resultTable_rows_selected
    s <- resultTableDetail[i,1]
    
    if (length(s)) {
      model <- allModelsResults[[s]]
      
      if(grepl("Dummy",as.character(resultTableDetail[i,2]))){
        dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == resultTableDetail[i,2])
        date <- as.Date(modelScopeDf$period)[which(modelScopeDf$period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & modelScopeDf$period <= dummyModelScopeDf[dummyModelIndex,"End_Date"])]
      }else {
        date <- as.Date(modelScopeDf$period)
      }
    }else {
      model <- allModelsResults[[1]]
      date <- as.Date(modelScopeDf$period)
    }
    
    p <- plot_ly()
    
    y_data <- input$colNames
    x_data  <- input$xAxis
    p <-add_trace(p,
                  x = date,
                  y = unname(predict(model)) ,
                  type = "scatter",
                  mode = "markers+lines" ,
                  name = "Fitted"
    )
    p <-add_trace(p,x = date, y = model$model[, 1] ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(model$model[1])))
    p <-add_trace(p,x = date, y = model$residuals ,type = "bar", mode = "hoverinfo" ,name = "Residual")
    for (name in y_data) {
      p <- add_trace( p,x = date, y = model$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
    }
    p %>% layout(legend = list(orientation = 'h'))
  })
  
  output$avmChart <- renderPlotly({
    baseModelAVMPlot()
  })
  
  baseModelAVMData <- reactive({
    i  <- input$resultTable_rows_selected
    s <- resultTableDetail[i,1]
    if (length(s)) {
      model <- allModelsResults[[s]]
      
      if(grepl("Dummy",as.character(resultTableDetail[i,2]))){
        dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == resultTableDetail[i,2])
        date <- as.Date(modelScopeDf$period)[which(modelScopeDf$period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & modelScopeDf$period <= dummyModelScopeDf[dummyModelIndex,"End_Date"])]
      }else {
        date <- as.Date(modelScopeDf$period)
      }
    }else {
      model <- allModelsResults[[1]]
      date <- as.Date(modelScopeDf$period)
    }
    avmData <- data.frame(Period = date, Actual = model$model[,1], Predicted = as.numeric(model$fitted.values), Residual = as.numeric(model$residuals))
  })
  
  output$avmData <- renderDataTable({
    datatable({
      baseModelAVMData()
    },rownames = FALSE, extensions = 'Buttons',options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX = TRUE,
      scrollY = TRUE,
      paging = FALSE,
      searching = FALSE,
      bInfo = FALSE
    ), selection = 'none')%>%
      formatRound(columns=c(2,3,4), digits=3)
  })
  
  output$modelDetails <- renderUI({
    i  <- input$resultTable_rows_selected
    if(!is.null(i)){
      s <- resultTableDetail[i,1]
    }else if(is.null(i)){
      s <- resultTableDetail[1,1]
    }else {
      return(0)
    }
    if (length(s) == 1) {
      tagList(
        fluidRow(
          box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
            dataTableOutput("parameters"),
            fluidRow(
              column(12, actionButton("addDummyButton","Add Dummy", icon = icon("list-alt"),style="color: #ffffff; background-color: #ec407a ; border-color: #212121; float:right"))
            )
          )
        ),
        fluidRow(
          box(title = "Actual V/S Predicted Plot",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = F,
            wellPanel(selectInput("colNames",label = "Columns",choices = names(allModelsResults[[s]]$model),multiple = T,selectize = T)),
            wellPanel(plotlyOutput("avmChart", width = "100%", height = "100%"))
          )
        ),
        fluidRow(
          box(title = "Actual V/S Predicted Data",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = T,
              wellPanel(dataTableOutput("avmData"))
          )
        )
      )
    } else {
      uiOutput("displayCompareModel")
    }
  })
  
  observeEvent(input$compareModel,{
    output$displayCompareModel <- renderUI({
      tagList(
        box(title = "Model Comparison", status = "info", solidHeader = T, width = 12,
            dataTableOutput("modelCompare"),
            fluidRow(
              column(4, downloadButton("downLoadCompareResults", "Download Compare Result",style="color: #212121; background-color: #f8bbd0; border-color: #212121"))
            )
        )
      )
    })
  })
  
  output$modelCompare = renderDataTable({
    i  <- input$resultTable_rows_selected
    indices <- resultTableDetail[i,1]
    comparedResult <<- compareModelResult(s=indices, allModelsResults, resultTableDetail, parametersDf)
    datatable({
      comparedResult
    },rownames = FALSE, extensions = 'Buttons', options = list(
      columnDefs = list(list(targets = "_all", searchable = TRUE)),
      dom = 'Bfrtip',autoWidth = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX = TRUE,scrollY = TRUE,paging = FALSE,searching = FALSE,bInfo = FALSE
    ), selection = 'none')%>% 
      formatRound(columns=c(2:7), digits=5)
  })
  
  observeEvent(input$amModelRefresh,{
    if(!is.null(resultTableDetail)){
      displayResultTable(resultTable)
      updateTabsetPanel(session, "inTabset",selected = "panel1")
      updateSelectizeInput(session, "sortVar",choices = c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","Model_Score","F_Stat","MAPE"),options = list(placeholder = 'Please select columns', onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  output$downLoadCompareResults <- downloadHandler(
    filename = function() {
      paste("CompareModelResult", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(x = comparedResult,file = file,row.names = F,quote = FALSE)
    }
  )
  
  output$downLoadResults <- downloadHandler(
    filename = function() {
      if(length(input$resultTable_rows_selected)==1){
        i  <- input$resultTable_rows_selected
        s <- resultTableDetail[i,1]
        paste(paste0(resultTable$`Model No`[s],"_results"), "zip", sep = ".")
      }else {
        my_slider_check_test <- "No model is selected, So 1st model result is downloading by default."
        js_string <- 'alert("SOMETHING");'
        js_string <- sub("SOMETHING",my_slider_check_test,js_string)
        session$sendCustomMessage(type='modelResultAlert', list(value = js_string))
        i  <- 1
        s <- resultTableDetail[i,1]
        paste(paste0(resultTable$`Model No`[s],"_results"), "zip", sep = ".")
      }
    },
    content = function(file) {
      if(length(input$resultTable_rows_selected)==1){
        i  <- input$resultTable_rows_selected
      }else{
        i  <- 1
      }
      s <- resultTableDetail[i,1]
      model <- allModelsResults[[s]]
      modelResult <- resultTable[s,]
      
      ModelDetail <- extractModelDetail(model, modelScopeDf, parametersDf, modelResult, RegDataTemp, dummyModelScopeDf)
      ModelDetail[["modelData"]] <- extractModelData(model,modelScopeDummyTable, modelScopeDf, parametersDf,dummyModelScopeDf,modelResult)
      
      # Writing all files as csv and zipped while downloading. Using Temp folder for writing csv files.
      wd <- getwd()
      setwd(tempdir())
      files <- NULL
      for(name in names(ModelDetail)){
        fileName <- paste0(name, ".csv")
        write.csv(x = ModelDetail[[name]], file = paste0(name,".csv"), row.names = FALSE,na = "",col.names = FALSE)
        files <- c(fileName,files)
      }
      zip(file,files)
      setwd(wd)
    }
  )
  
  output$downLoadData <- downloadHandler(
    filename = function() {
      paste("TransformedData", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(x = finalDf,file = file,row.names = F,quote = FALSE)
    }
  )
  
  ################################### AM Dummy Model ###########################################
  #--------------------------------------------------------------------------
  # Function to add dummy variable to candidate model.
  #--------------------------------------------------------------------------
  
  dummyModel <- NULL
  # to open the pop up for dummy variable addition
  observeEvent(input$addDummyButton, {
    shinyjs::hide(id = "dummyModelAvmChart")
    shinyjs::hide(id = "dummyModelParameters")
    shinyjs::hide(id = "dummyModelResult")
    updateTabsetPanel(session, "inTabset", selected = "panel2")
  })
  
  output$DummyModelScopeUI <- renderUI({
    start_date <- input$startDate
    end_date <- input$endDate
    tagList(
      fluidRow(
        box(title = "Base Model AVM",status = "success",width = "6",solidHeader = T,collapsible = F,
            wellPanel(plotlyOutput("avmChartBase", width = "100%", height = "100%"))
        ),
        box(title = "Dummy AVM",status = "success",width = "6",solidHeader = T,collapsible = F,
            wellPanel(plotlyOutput("dummyModelAvmChart", width = "100%", height = "100%"))
        )
      ),
      fluidRow(
        box(title = "Base Model Result",status = "success",width = "6",solidHeader = T,collapsible = F,
            dataTableOutput("baseParameters")
        ),
        box(title = "Dummy Model Result",status = "success",width = "6",solidHeader = T,collapsible = F,
            dataTableOutput("dummyModelParameters")  
        )
      ),
      fluidRow(
        box(title = "Model Parameter",status = "success",solidHeader = T,width = 6,
            dataTableOutput("baseModelResult")
        ),
        box(title = "Dummy Model Parameter",status = "success",solidHeader = T,width = 6,
            dataTableOutput("dummyModelResult"),
            uiOutput("addDummyModel")
        )
      ),
      fluidRow(
        box(title = "Dummy Addition",status = "warning",width = "12",solidHeader = T,
            fluidRow(
              column(5,dateInput('dummyStartDate', label = "Modelling start Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = start_date,min = start_date,max = end_date)),
              column(5,dateInput('dummyEndDate', label = "Modelling end Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = end_date,min = start_date,max = end_date)),
              column(2,actionButton("dummyModelScope", "Continue",style="color: #212121; background-color: #fbc998; border-color: #2e6da4"))
            ),
            uiOutput("DummyVarTableUI")
        )
      )
    )
  })
  
  output$avmChartBase <- renderPlotly({
    baseModelAVMPlot()
  })
  
  output$baseParameters = renderDataTable({
    baseModelParameter()
  })
  
  output$baseModelResult = renderDataTable({
    if(is.null(input$resultTable_rows_selected)){
      model.index <<- 1
    }else{
      model.index <<- as.numeric(rownames(resultTableDetail[input$resultTable_rows_selected,]))
    }
    resultTable[model.index,c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")] <- round(resultTable[model.index,c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")],digits = 5)
    baseModel <- t(resultTable[model.index,c("Model No","%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")])
    baseModel <- data.frame(Paramater = rownames(baseModel),Value = baseModel, row.names = NULL)
    names(baseModel) <- c("Paramater","Value")
    
    datatable({
      baseModel
    },rownames = FALSE, extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',autoWidth = FALSE,columnDefs = list(list(width = "120px", targets = "_all")),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      paging = FALSE,scrollX = FALSE,scrollY = FALSE,searching = FALSE
    ), selection = 'none')
  })
  
  observeEvent(input$dummyModelScope,{
    output$DummyVarTableUI <- renderUI({
      tagList(
        box(title = "Provide Dummy Model Variable value",status = "warning",width = "12",solidHeader = T,
            wellPanel(
              rHandsontableOutput("DummyVarTable"),
              br(),
              fluidRow(
                column(12, actionButton("submitDummyVar","Generate Result", icon = icon("table"),style="color: #212121; background-color: #ffcdd2; border-color: #212121; float: right"))
              )
            )
        )
      )
    })
  })
  
  output$DummyVarTable <- renderRHandsontable({
    modelScopeDummyTable <- dummyData()
    if (!is.null(modelScopeDummyTable)){
      rhandsontable(modelScopeDummyTable, 
                    height = 400,useTypes = TRUE,stretchH = "all",selectCallback = T,
                    fillHandle = list(direction='vertical', autoInsertRow=FALSE), 
                    maxRows = nrow(modelScopeDummyTable)) %>%
        hot_col("Period", readOnly = TRUE)%>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  dummySDateTmp <- NULL
  dummyEDateTmp <- NULL
  
  dummyData = reactive({
    if(!is.null(input$DummyVarTable)){
      dummyDF <- hot_to_r(input$DummyVarTable)
      if(dummySDateTmp != input$dummyStartDate | dummyEDateTmp != input$dummyEndDate){
        dummyModelScopeDf <- subset(modelScopeDf, period >= input$dummyStartDate & period <= input$dummyEndDate)
        dummyDF <- data.frame(
          Period = dummyModelScopeDf$period,
          Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          row.names = c(1:nrow(dummyModelScopeDf)),
          stringsAsFactors = FALSE )
        dummySDateTmp <<- input$dummyStartDate
        dummyEDateTmp <<- input$dummyEndDate
      }
    }else {
      if (is.null(values[["dummyDF"]])){
        dummyModelScopeDf <- subset(modelScopeDf, period >= input$dummyStartDate & period <= input$dummyEndDate)
        dummyDF <- data.frame(
          Period = dummyModelScopeDf$period,
          Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
          row.names = c(1:nrow(dummyModelScopeDf)),
          stringsAsFactors = FALSE )
        dummySDateTmp <<- input$dummyStartDate
        dummyEDateTmp <<- input$dummyEndDate
      }
      else
        dummyDF = values[["dummyDF"]]
    }
    values[["dummyDF"]] = dummyDF
    dummyDF
  })
  
  # to close the pop up for dummy variable addition
  observeEvent(input$submitDummyVar, {
    shinyjs::show(id = "dummyModelAvmChart")
    shinyjs::show(id = "dummyModelParameters")
    shinyjs::show(id = "dummyModelResult")
    
    if(!is.null(input$addDummyButton)){
      modelScopeDummyTable <<- isolate(values[["dummyDF"]])
      
      if(is.null(input$resultTable_rows_selected)){
        model.index <<- 1
      }else{
        model.index <<- as.numeric(rownames(resultTableDetail[input$resultTable_rows_selected,]))
      }
      
      if (sum(as.vector(apply(modelScopeDummyTable[,-1],2,sum))) == 0){
        meCustomAlert(message = "Please provide values to atleast one dummy variable.",alertType = "warning")
      }else {
        model <- allModelsResults[[model.index]]
        dummyModel <<- getDummyModelResult(model, modelScopeDummyTable, RegDataTemp, modelScopeDf)
        dummyModelList <- NULL
        
        output$dummyModelResult = renderDataTable({
          dummyModelList[[1]] <- dummyModel
          dummyModelResult <- as.data.frame(extractModelParameter(dummyModelList))
          dummyModelResult$`Model No` <- "Dummy Model"
          dummyModelResult[,c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")] <- round(dummyModelResult[,c("%R2","%R2.adj","2-DW","T.stat.avg","VIF.Avg","RootMSE","F_Stat","MAPE")],digits = 5)
          dummyModel <- t(dummyModelResult[,-1])
          dummyModel <- data.frame(Paramater = rownames(dummyModel),Value = dummyModel, row.names = NULL)
          names(dummyModel) <- c("Paramater","Value")
          
          datatable({
            dummyModel
          },rownames = FALSE, extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',autoWidth = FALSE,columnDefs = list(list(width = "120px", targets = "_all")),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            paging = FALSE,scrollX = FALSE,scrollY = FALSE,searching = FALSE
          ), selection = 'none')
        })
        
        output$dummyModelParameters = renderDataTable({
          datatable({
            dummyModelResultDetail <- getElasticity(dummyModel,parametersDf = parametersDf)
          },rownames = FALSE, extensions = 'Buttons',options = list(
            dom = 'Bfrtip',autoWidth = TRUE,columnDefs = list(list(width = "120px", targets = "_all")),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            paging = FALSE,scrollX = TRUE,scrollY = TRUE,searching = FALSE
          ), selection = 'none')%>%
            formatRound(columns=c(2,3,4,5,6,7,8,9), digits=5) 
        })
        
        output$dummyModelAvmChart <- renderPlotly({
          date <- as.Date(modelScopeDummyTable$Period)
          p <- plot_ly()
          
          y_data <- input$topColNames
          x_data  <- input$xAxis
          p <-add_trace(p,
                        x = date,
                        y = unname(predict(dummyModel)) ,
                        type = "scatter",
                        mode = "markers+lines" ,
                        name = "Fitted"
          )
          p <-add_trace(p,x = date, y = dummyModel$model[, 1] ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(dummyModel$model[1])))
          p <-add_trace(p,x = date, y = dummyModel$residuals ,type = "bar", mode = "hoverinfo" ,name = "Residual")
          for (name in y_data) {
            p <- add_trace( p,x = date, y = dummyModel$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
          }
          p %>% layout(legend = list(orientation = 'h'))
        })
        
        output$addDummyModel <- renderUI({
          tagList(
            actionButton("dummyModelSubmit","Submit",style="color: #212121; background-color: #ffc8b3; border-color: #212121; float: right")
          )
        })
      }
    }else {
      meCustomAlert(message = "No Model selected!, Please select a model after regression by clicking on 'Add Dummy'.",alertType = "warning")
      toggleModal(session, "dummyModel", toggle = "close")
    }
  })
  
  dummyModelScopeDf <- NULL
  
  observeEvent(input$dummyModelSubmit,{
    modelNumber <- as.numeric(gsub("_.*","",gsub("CANDIDATE_","",resultTable[model.index,"Model No"])))
    allModelsResults[[length(allModelsResults)+1]] <<- dummyModel
    resultList <<- updateAllModelsResults(allModelsResults,modelNumber,resultTable, allModelsList, input$rankType)
    resultTable <<- resultList[["ranked_result"]]
    allModelsList <<- resultList[["allModelsList"]]
    dummytmp <- data.frame(as.character(resultTable[nrow(resultTable),which(names(resultTable) %in% "Model No")]),input$dummyStartDate,input$dummyEndDate)
    names(dummytmp) <- c("Model_No","Start_Date","End_Date")
    dummyModelScopeDf <<- rbind(dummyModelScopeDf,dummytmp)
    
    displayResultTable(resultTable)
    updateTabsetPanel(session, "inTabset",selected = "panel1")
    updateSelectizeInput(session, "sortVar",choices = c("%R2","%R2.adj","DW","T.stat.avg","VIF.Avg","RootMSE","Model_Score","F_Stat","MAPE"),options = list(placeholder = 'Please select columns', onInitialize = I('function() { this.setValue(""); }')))
  })
  
  ################################### AM Filter Model ###########################################
  #--------------------------------------------------------------------------
  # Function is for displaying bucket wise variable and model filter through new tab.
  #--------------------------------------------------------------------------
  
  output$displayAM_FilterResult <- renderUI({
    fluidRow(
      box(title = "Select Variables to Filter The Models",status = "success",width = "12",solidHeader = T,collapsible = F,
          fluidRow(
            column(12,dataTableOutput("bucketVarData"))
          ),
          fluidRow(
            column(4, actionButton("submitFilterModel","Submit", icon = icon("table"),style="color: #212121; background-color: #ffcdd2; border-color: #212121"))
          )
      ),
      box(title = "Filter Result Output",status = "success",width = "12",solidHeader = T,collapsible = F,
          DT::dataTableOutput("filterResultTable"),
          br(),
          fluidRow(
            column(4, downloadButton("downLoadFilterResults", "Download Model",style="color: #212121; background-color: #f8bbd0; border-color: #212121"))
          )
      ),
      uiOutput("displayFilteredModelDetail")
    )
  })
  
  bucketVarData <<- data.frame()
  
  output$bucketVarData <- DT::renderDataTable({
    bucketVarData <<- createBucketVarData(amInputBuckets)
  }, server = TRUE, class = 'cell-border stripe',rownames = FALSE, 
  selection = list(mode = 'multiple',target = 'cell'),options = list(pageLength = nrow(bucketVarData))
  )
  
  observeEvent(input$submitFilterModel, {
    filteredModelResultsTable <<- data.frame()
    selectedCell <- as.data.frame(input$bucketVarData_cells_selected)
    
    if(length(selectedCell)==0){
      meCustomAlert(message = "Please select atleast one variable from each bucket including all the variables from Dependent bucket.",alertType = "warning")
    }else {
      
      withProgress(message = 'Model Filtering is inprogress.', detail = 'This may take a while..., please be patient.', value = 0.85, {
        selectedCell$V2 <- selectedCell$V2 +1
        bucketSelectedVarName <- as.vector(NULL)
        for (i in 1:nrow(selectedCell)) {
          bucketSelectedVarName <- append(bucketSelectedVarName,as.character(bucketVarData[selectedCell$V1[i],selectedCell$V2[i]]))
        }
        bucketSelectedVarName <- c(parametersDf$VariableName[parametersDf$Bucket == "Dependent"], bucketSelectedVarName)
        modelVar <- extractModelVarName(allModelsResults)
        filteredModelResultsTable <<- extractFilterModelResults(bucketSelectedVarName, modelVar, allModelsResults, resultTable)
        
        if(length(filteredModelResultsTable) != 1 & nrow(filteredModelResultsTable) > 1){
          ranked_FilteredResult <- sortModelResult(filteredModelResultsTable, flag = 1)
          filteredModelResultsTable <<- ranked_FilteredResult
        }
        displayFilterResultTable(filteredModelResultsTable)
        output$displayFilteredModelDetail <- renderUI({
          uiOutput("filterModelDetails")
        })
      })
    }
  })
  
  displayFilterResultTable <- function(filteredModelResultsTable){
    
    filteredModelResultsTable <<- filteredModelResultsTable
    
    output$filterResultTable <- renderDataTable(
      server = TRUE,
      datatable({
        if(length(filteredModelResultsTable) == 0){
          meCustomAlert(message = "Modeller Engine doesn't have any model with satisfying variables filter criteria. Please select another variables combination.",alertType = "warning")
        }else {
          filteredModelResultsTable[,-1]
        }
      }, rownames = FALSE, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',autoWidth = TRUE,columnDefs = list(list(width = "120px", targets = "_all")),
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),paging = TRUE,scrollX = TRUE,scrollY = TRUE,searching = FALSE
      ), selection = list(mode = "single"), height = "2000px")
    )
  }
  
  output$filterModelDetails <- renderUI({
    i  <- input$filterResultTable_rows_selected
    if(!is.null(i)){
      ms <- filteredModelResultsTable[i,1]
    }else{
      ms <- filteredModelResultsTable[1,1]
    }
    if (length(ms) == 1) {
      tagList(
        box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
            dataTableOutput("filterParameters")
        ),
        box(title = "Actual V/S Predicted",status = "success",solidHeader = T,width = 12,
            wellPanel(
              selectInput("colNames",label = "Columns",choices = names(allModelsResults[[ms]]$model),multiple = T,selectize = T
              )),
            wellPanel(
              plotlyOutput("filterModelAvmChart", width = "100%", height = "100%")
            ))
      )
    } 
  })
  
  output$filterParameters = renderDataTable({
    i  <- input$filterResultTable_rows_selected
    ms <- filteredModelResultsTable[i,1]
    if(!length(ms)){
      ms <- filteredModelResultsTable[1,1]
    }
    
    datatable({
      filterResultDetail <- getElasticity(allModelsResults[[ms]],parametersDf = parametersDf)
      filterResultDetail[,2:9] <- sapply(filterResultDetail[,2:9], function(x) round(x, digits = 5))
      filterResultDetail
      
    },rownames = FALSE, extensions = 'Buttons',options = list(
      scrollX = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollY = TRUE,
      paging = FALSE,
      searching = FALSE,
      bInfo = FALSE
    ), selection = 'none')
  })
  
  output$filterModelAvmChart <- renderPlotly({
    i  <- input$filterResultTable_rows_selected
    ms <- filteredModelResultsTable[i,1]
    
    if (!length(ms)) {
      i <- 1
      ms <- filteredModelResultsTable[1,1]
    }
    filteredModel <- allModelsResults[[ms]]
    
    if(grepl("Dummy",as.character(filteredModelResultsTable[i,2]))){
      dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == filteredModelResultsTable[i,2])
      date <- as.Date(modelScopeDf$period)[which(modelScopeDf$period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & modelScopeDf$period <= dummyModelScopeDf[dummyModelIndex,"End_Date"])]
    }else {
      date <- as.Date(modelScopeDf$period)
    }
    
    p <- plot_ly()
    
    y_data <- input$colNames
    x_data  <- input$xAxis
    p <-add_trace(p,
                  x = date,
                  y = unname(predict(filteredModel)) ,
                  type = "scatter",
                  mode = "markers+lines" ,
                  name = "Fitted"
    )
    p <-add_trace(p,x = date, y = filteredModel$model[, 1] ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(filteredModel$model[1])))
    p <-add_trace(p,x = date, y = filteredModel$residuals ,type = "bar", mode = "hoverinfo" ,name = "Residual")
    for (name in y_data) {
      p <- add_trace( p,x = date, y = filteredModel$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
    }
    p %>% layout(legend = list(orientation = 'h'))
  })
  
  output$downLoadFilterResults <- downloadHandler(
    filename = function() {
      if(length(input$filterResultTable_rows_selected)==1){
        i  <- input$filterResultTable_rows_selected
        ms <- filteredModelResultsTable[i,1]
        paste(paste0(filteredModelResultsTable$`Model No`[i],"_results"), "zip", sep = ".")
      }else {
        i  <- 1
        ms <- filteredModelResultsTable[i,1]
        paste(paste0(filteredModelResultsTable$`Model No`[i],"_results"), "zip", sep = ".")
      }
    },
    content = function(file) {
      if(length(input$filterResultTable_rows_selected)==1){
        i  <- input$filterResultTable_rows_selected
      }else{
        i  <- 1
      }
      ms <- filteredModelResultsTable[i,1]
      model <- allModelsResults[[ms]]
      modelResult <- resultTable[ms,]
      
      ModelDetail <- extractModelDetail(model, modelScopeDf, parametersDf, modelResult, RegDataTemp, dummyModelScopeDf)
      ModelDetail[["modelData"]] <- extractModelData(model,modelScopeDummyTable, modelScopeDf, parametersDf,dummyModelScopeDf,modelResult)
      
      # Writing all files as csv and zipped while downloading. Using Temp folder for writing csv files.
      wd <- getwd()
      setwd(tempdir())
      files <- NULL
      for(name in names(ModelDetail)){
        fileName <- paste0(name, ".csv")
        write.csv(x = ModelDetail[[name]], file = paste0(name,".csv"), row.names = FALSE,na = "",col.names = FALSE)
        files <- c(fileName,files)
      }
      zip(file,files)
      setwd(wd)
    }
  )
  
  ################################### AM TOP Model Filter ###########################################
  #--------------------------------------------------------------------------
  # Function is for displaying top model for each variable combination through bscollapse panel.
  #--------------------------------------------------------------------------
  
  output$displayAM_TopResult <- renderUI({
    box(title = "Select Variable Combination to Filter The Top Model",status = "success",width = "12",solidHeader = T,collapsible = F,
        wellPanel(
          fluidRow(
            column(3, actionButton("topModelButton","Click here to Get Top Models", icon = icon("list-alt"),style="color: #ffffff; background-color: #ec407a ; border-color: #ffffff; float:left")),
            column(9, htmlOutput("getTopModelButton"))
          )
        ),
        br(),
        uiOutput("topModelsDisplay"),
        uiOutput("topModelDetailPop")
    )
  })
  
  observeEvent(input$topModelButton, {
    output$topModelsDisplay <- renderUI({
      withProgress(message = 'Fetching the Top Model for Each Variable Combinations.', 
                   detail = 'This may take a while..., please be patient.', value = 0.85, {
                     topModelResult <- extractTopModelVariableCombResult(allModelsResults, resultTableDetail, resultTable)
                     topTable <<- as.data.frame(rbindlist(sapply(topModelResult, function(x) topDF <- resultTable[x,],simplify = FALSE),fill = TRUE))
                     topTable[,3:7] <- sapply(topTable[,3:7], function(x) round(x, digits = 5))
                     # create datatables
                     observe(
                       lapply(seq_len(nrow(topTable)), function(i) {
                         output[[paste0("topResultTable",i)]] <- DT::renderDataTable({
                           datatable({
                             topTable[i,-1]
                           }, rownames = FALSE, class = 'cell-border stripe',extensions = 'Buttons',
                           options = list(columnDefs = list(list(width = "120px", targets = "_all")),
                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                          paging = FALSE,
                                          searching = FALSE,
                                          bInfo = FALSE
                           ), selection = list(mode = "multiple"), height = "2000px")
                         })
                       })  
                     )  
                     
                     col_list <- sapply(1:nrow(topTable), function(i){
                       interp(
                         quote(
                           bsCollapsePanel(paste0("Variable Combination ",i), "Variable Combination with :", strong(p(paste(varCombList[[i]], collapse = "  |  "))),br(),DT::dataTableOutput(paste0("topResultTable",i)), style = "info")
                         ),.values = list(i=i, nm = "Variable Combination 1",val='')
                       )
                     })
                     panels <- list(id = "topCollapsePanel", col_list)
                     do.call(bsCollapse, unlist(panels))
                   })
    })
    
    output$getTopModelButton <- renderUI({
      tagList(
        fluidRow(
          column(6, actionButton("getModelDetail","Click here for Top Model Detail", icon = icon("list-alt"),style="color: #ffffff; background-color: #42a5f5 ; border-color: #ffffff;float:center")),
          column(4, downloadButton("downLoadTopModelDetail", "Click here to Download Model Detail",style="color: #ffffff; background-color: #ffa726 ; border-color: #ffffff;float:right"))
        )
      )
    })
  })
  
  output$downLoadTopModelDetail <- downloadHandler(
    filename = function() {
      if(!is.null(input$topCollapsePanel)){
        panel.number <- as.numeric(gsub("Variable Combination ","", input$topCollapsePanel))
        model.number <- topTable$Index[panel.number]
        paste(paste0("Variable_Combination_",panel.number,"_",topTable[panel.number,2],"_Result"), "zip", sep = ".")
      }else {
        meCustomAlert(message = "No Variable combination is selected.",alertType = "warning")
      }
    },
    content = function(file) {
      if(!is.null(input$topCollapsePanel)){
        panel.number <- as.numeric(gsub("Variable Combination ","", input$topCollapsePanel))
        model.number <- topTable$Index[panel.number]
        model <- allModelsResults[[model.number]]
        modelResult <- resultTable[model.number,]
        
        ModelDetail <- extractModelDetail(model, modelScopeDf, parametersDf, modelResult, RegDataTemp, dummyModelScopeDf)
        ModelDetail[["modelData"]] <- extractModelData(model,modelScopeDummyTable, modelScopeDf, parametersDf,dummyModelScopeDf,modelResult)
        
        # Writing all files as csv and zipped while downloading. Using Temp folder for writing csv files.
        wd <- getwd()
        setwd(tempdir())
        files <- NULL
        for(name in names(ModelDetail)){
          fileName <- paste0(name, ".csv")
          write.csv(x = ModelDetail[[name]], file = paste0(name,".csv"), row.names = FALSE,na = "",col.names = FALSE)
          files <- c(fileName,files)
        }
        zip(file,files)
        setwd(wd)
      }
    }
  )
  
  observeEvent(input$getModelDetail,{
    if(!is.null(input$topCollapsePanel)){
      toggleModal(session, "topModel", toggle = "open")
    }else {
      meCustomAlert(message = "No Variable combination is selected.",alertType = "warning")
    }
  })
  
  output$topModelDetailPop <- renderUI({
    bsModal(id = "topModel",title = "Top Model Detail for Selected Variable Combination", trigger = "getModelDetail",size = "large",
            tagList(
              box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
                  dataTableOutput("topModelParameters")
              ),
              box(title = "Actual V/S Predicted",status = "success",solidHeader = T,width = 12,
                  wellPanel(
                    plotlyOutput("topModelAvmChart", width = "100%", height = "100%")
                  ))
            )
    )
  })
  
  output$topModelParameters = renderDataTable({
    datatable({
      panel.number <- as.numeric(gsub("Variable Combination ","", input$topCollapsePanel))
      model.number <- topTable$Index[panel.number]
      if (length(model.number)) {
        topModelResultDetail <- getElasticity(allModelsResults[[model.number]],parametersDf = parametersDf)
        topModelResultDetail[,c(2:9)] <- sapply(topModelResultDetail[,c(2:9)], function(x) round(x, digits = 5))
        topModelResultDetail
      }
    },rownames = FALSE, extensions = 'Buttons',options = list(
      scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),paging = FALSE,searching = FALSE,bInfo = FALSE
    ), selection = 'none') 
  })
  
  output$topModelAvmChart <- renderPlotly({
    panel.number <- as.numeric(gsub("Variable Combination ","", input$topCollapsePanel))
    model.number <- topTable$Index[panel.number]
    
    topModel <- allModelsResults[[model.number]] 
    
    if(grepl("Dummy",as.character(topTable[panel.number,2]))){
      dummyModelIndex <- which(as.character(dummyModelScopeDf$Model_No) == topTable[panel.number,2])
      date <- as.Date(modelScopeDf$period)[which(modelScopeDf$period >= dummyModelScopeDf[dummyModelIndex,"Start_Date"] & modelScopeDf$period <= dummyModelScopeDf[dummyModelIndex,"End_Date"])]
    }else {
      date <- as.Date(modelScopeDf$period)
    }
    
    p <- plot_ly()
    
    y_data <- input$topColNames
    x_data  <- input$xAxis
    p <-add_trace(p,
                  x = date,
                  y = unname(predict(topModel)) ,
                  type = "scatter",
                  mode = "markers+lines" ,
                  name = "Fitted"
    )
    p <-add_trace(p,x = date, y = topModel$model[, 1] ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(topModel$model[1])))
    p <-add_trace(p,x = date, y = topModel$residuals ,type = "bar", mode = "hoverinfo" ,name = "Residual")
    for (name in y_data) {
      p <- add_trace( p,x = date, y = topModel$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
    }
    p %>% layout(legend = list(orientation = 'h'))
  })
  
  ####################################
  #####Variable Console Code##########
  ####################################
  
  ##################################################
  #####Variable Console Code for CSV input##########
  ##################################################
  MasterDF <- NULL
  varTag <- NULL
  values <- reactiveValues(formula="", showAddVariable = 1)
  column <- as.vector(NULL)
  formString <-NULL
  depList <- as.list(NULL)
  depTable <- data.frame(var = character(0), exp = character(0), isformula=logical(0))
  indepList <- as.list(NULL)
  indepTable <- data.frame(var = character(0), exp = character(0), isformula=logical(0))
  downloadMasterDF <- NULL
  
  observeEvent(input$variableCsvFile,{
    withProgress(message = 'Uplaoding the file...', value = 0.85, {
      varCsv <- input$file2
      if (is.null(varCsv))
        return(NULL)
      varDataTemp <-read.csv(varCsv$datapath,header = TRUE,stringsAsFactors = FALSE)
      varTag <<- data.table(variableName = rownames(t(varDataTemp[1, ])),bucket = as.character(t(varDataTemp[1, ])),stringsAsFactors = F)
      MasterDF <<- variableConsoleFile(varDataTemp, "inputFile", varTag)
      
      updateSelectizeInput(session = session, "column",choices = colnames(MasterDF),selected = NULL)
      updateSelectizeInput(session = session, "icolumn",choices = colnames(MasterDF),selected = NULL)
      updateSelectizeInput(session = session, "col",choices = colnames(MasterDF),selected = NULL)
      updateSelectizeInput(session = session, "icol",choices = colnames(MasterDF),selected = NULL)
      displayMasterTable(MasterDF)
    })
  })
  
  # function to display master data table.
  displayMasterTable <- function(table){
    output$vofTable <- renderDataTable(datatable({
      MasterDF
    }, rownames = FALSE, extensions = 'Buttons', options = list(
      #dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      paging = TRUE,
      scrollX = TRUE,
      searching = FALSE,
      columnDefs = list(list(width = '120px', targets = "_all"))
    ), selection = 'none')
    )
  }
  
  observeEvent(input$forward_UploadCsv,{
    updateTabItems(session, "sidebartabs", "directCsvUpload")
  })
  
  ################################### VC DepVar ###########################################
  #............................................................................
  # Function related to dependent variable.
  #............................................................................
  
  # Display actionButton based on isFormula checkbox to add record in dependent table..
  output$button_ui <- renderUI({
    if(input$isFormula == TRUE){
      actionButton("depSubmitWithFormula", "Submit")
    }
    else{
      actionButton("depSubmitWithoutFormula", "Submit")
    }
  })
  
  # display popup while isFormula Checkbox is open.
  output$depPop <- renderUI({
    tagList(
      bsModal("modal", "Add Formula for Dependent Variable", trigger = "depSubmitWithFormula",
              wellPanel(h4("Write a formula to display"),br(),
                wellPanel(
                  textInput("formula", "Formula:",value = NULL),
                  actionButton("submitFormula","Submit Formula")
                ),br(),
                
                wellPanel(
                  selectizeInput('col', 'Select Column: ', choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
                  ),
                  actionButton("goButton", "Select Column")
                ))
      )
    )
  })
  
  #Observe the event when formula is false for dependent variable.
  observeEvent(input$depSubmitWithoutFormula,{
    
    if(input$tabs == "dep"){   # for dependent variable
      if(input$variable != "" && input$column != ""){  # checking whether variable name and column are missing.
        if(nrow(depTable) == 0){  # Checking there should not be any dep variable already present.
          insertdepVarWF("insert")
          updateTextInput(session, "variable", value = "")
        }
        else{
          index <- which(depTable$V1 == input$variable)
          len <- length(index)
          if(len == 1){
            insertdepVarWF("update")
          }
          else{
            displayAlert("alert4")
          }
        }
      }
      else{
        # if input variable is missing.
        if(nrow(depTable) == 0){
          displayAlert("alert1")
        }
        # if already one depenedent variable is present in table.
        else{
          index <- which(depTable$V1 == input$variable)
          len <- length(index)
          # alert if column input is missing to modify the variable.
          if(len == 1){
            displayAlert("alert7")
          }
          # alert if want to insert more than one dep variable.
          else{
            displayAlert("alert4")
          }
        }
      }
    }
    # reseting the UI input fields.
    updateTextInput(session, "variable", value = "")
  })
  
  # If one dependent variable is already present in the record then user should not be able to insert new formula.
  observeEvent(input$depSubmitWithFormula,{
    if(nrow(depTable) != 0){
      if(input$variable != ""){
        if(depTable$V1[1] != input$variable){
          updateTextInput(session, "variable", value = "")
          # one variable is already present, can't insert another variable.
          displayAlert("alert4")
        }
        else{
          # if variable is already present, and user wants to modify then popup will open.
          toggleModal(session, "modal", toggle = "open")
        }
      }
      else {
        # if variable is missing
        displayAlert("alert2")
      }
    }
    else if(nrow(depTable) == 0){
      # if no variable is present in depTable.
      if(input$variable != ""){
        toggleModal(session, "modal", toggle = "open")
      }
      else{
        # if variable is missing
        displayAlert("alert2")
      }
    }
  })
  
  # Observe the event while submiting the formula from pop-up window for dependent variable.
  observeEvent(input$submitFormula,{
    
    if(input$tabs == "dep" && input$variable != ""){
      if(nrow(depTable) == 0){
        # Taking formula through pop and storing into depTable with variable name and isFormula
        if(formulaValidate(formString) == "Error"){
          displayAlert("alert3")
        }
        else{
          insertdepVar("insert")
        }
      }
      else{
        index <- which(depTable$V1 == input$variable)
        len <- length(index)
        if(len == 1){
          if(formulaValidate(formString) == "Error"){
            displayAlert("alert3")
          }
          else{
            insertdepVar("update")
          }
        }
      }
    }
    else {
      displayAlert("alert2")
    }
    updateTextInput(session,inputId = "formula",value = "")
    updateTextInput(session, "variable", value = "")
    toggleModal(session = session,modalId = "modal", toggle = "close")
  })
  
  # observe the event while selecting the column to make formula in pop-up window.
  observeEvent(input$goButton,{
    column <- unlist(isolate(input$col))
    formString <<- input$formula
    formString <<- paste(formString,column)
    depList[[input$variable]][1] <- formString
    updateSelectInput(session = session, "col",choices = colnames(MasterDF)[-1],selected = NULL)
    updateTextInput(session,inputId = "formula",value = formString)
  })
  
  # function to insert the variable in depTable using depList without formula.
  insertdepVarWF <- function(action){
    depList[[input$variable]] <<- c(input$variable,input$column, input$isFormula)
    depTable <<- as.data.frame(t(as.data.frame.list(depList)))
    temp <- with(MasterDF,eval(parse(text = as.character(input$column))))
    
    displaydeptable(depTable)
    if(action == "insert"){
      insertdepTable(temp)
    }else if(action == "update"){
      updatedepTable(temp)
    }
  }
  
  # function to insert the variable in depTable using depList.
  insertdepVar <- function(action){
    depListTemp <- as.list(NULL)
    depTableTemp <- as.data.frame(NULL)
    depListTemp[[input$variable]] <- c(input$variable,formString, input$isFormula)
    depTableTemp <- as.data.frame(t(as.data.frame.list(depListTemp)))
    temp <- with(MasterDF,eval(parse(text = as.character(depTableTemp[1,2]))))
    # Validate the new variable result, whether column with same result present in master table.
    matchCol <- columnCheck(MasterDF,temp)
    if(matchCol > 0){
      colPresentAlert(matchCol)
    }else{
      depList[[input$variable]] <<- depListTemp[[input$variable]]
      depTable <<- as.data.frame(t(as.data.frame.list(depList)))
      displaydeptable(depTable)
      if(action == "insert"){
        insertdepTable(temp)
      }else if(action == "update"){
        updatedepTable(temp)
      }
    }
  }
  
  # Function to insert the dependent variable as column into master table.
  insertdepTable <- function(temp){
    MasterDF$temp <<- temp
    colnames(MasterDF)[which(names(MasterDF) == "temp")] <<- input$variable
    updateColumnDropDown(session)
    displayMasterTable(MasterDF)
  }
  
  # Function to update the dependent column which is already present in the master table.
  updatedepTable <- function(temp){
    MasterDF[,which(names(MasterDF) == input$variable)] <<- temp
    displayMasterTable(MasterDF)
  }
  
  # Fucntion to delete the formula from dependent variable and simultaneously updating the master table.
  observeEvent(input$delete, {
    s = input$deptable_rows_selected
    row_name <- rownames(depTable)[s]
    if(!is.null(s)){
      delCol <- depTable[s,1]
      depList[row_name] <<- NULL
      depTable <<- depTable[-s,]
      if (length(s)) {
        displayAlert("alert5")
      }
      displaydeptable(depTable)
      MasterDF <<- MasterDF[,!colnames(MasterDF) %in% row_name]
      updateColumnDropDown(session)
      displayMasterTable(MasterDF)
    }
  })
  
  # function for displaying indepTable of dependent variable.
  displaydeptable <- function(depTable){
    if(nrow(depTable)==0){
      depTable <- NULL
    }else {
      colnames(depTable) <- c("Variable_Name", "Expersion/DB_Variable", "IsFromula")
      depTable <- data.frame(depTable)
    }
    output$deptable <- DT::renderDataTable({
      depTable
    },server = TRUE, rownames = FALSE)
  }
  
  ################################### VC InDepVar ###########################################
  #............................................................................
  # Function related to independent variable.
  #............................................................................
  
  # Display actionButton based on isFormula checkbox is true then popup for formula otherwise submit button to add record in independent variables.
  output$ibutton_ui <- renderUI({
    if(input$iisFormula == TRUE){
      actionButton("indepSubmitWithFormula", "Submit")
    }
    else{
      actionButton("indepSubmitWithoutFormula", "Submit")
    }
  })
  
  # display popup while isFormula Checkbox is open.
  output$indepPop <- renderUI({
    tagList(
      bsModal("imodal", "Add Formula for Independent Variable", trigger = "indepSubmitWithFormula",
              wellPanel(
                h4("Write a formula to display"),br(),
                wellPanel(
                  textInput("iformula", "Formula:",value = NULL),
                  actionButton("isubmitFormula","Submit Formula")
                ),br(),
                
                wellPanel(
                  selectizeInput('icol', 'Select Column: ', choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
                  ),
                  actionButton("igoButton", "Select Column")
                ))
      )
    )
  })
  
  # Observe the event when formula is false for independent variable.
  observeEvent(input$indepSubmitWithoutFormula,{
    if(input$tabs == "indep"){
      if(input$ivariable != "" && input$icolumn != "" && input$iisFormula == FALSE){
        index <- which(indepTable$V1 == input$ivariable)
        len <- length(index)
        if(len == 0){
          insertIndepVarWF("insert")
        }
        else {
          insertIndepVarWF("update")
        }
      }
      else{
        displayAlert("alert1")
      }
    }
    updateTextInput(session, "ivariable", value = "")
    updateSelectInput(session = session, "icolumn",choices = colnames(MasterDF),selected = NULL) 
  })
  
  # Observe the event when formula is true for independent variable.
  observeEvent(input$indepSubmitWithFormula,{
    if(input$ivariable != ""){
      # if variable is already present, and user wants to modify then popup will open.
      toggleModal(session, "imodal", toggle = "open")
    }
    else {
      # if variable is missing
      displayAlert("alert2")
    }
  })
  
  # Observe the event while submiting the formula from pop-up window for independent variable.
  observeEvent(input$isubmitFormula,{
    if(input$tabs == "indep" && input$ivariable != ""){
      index <- which(indepTable$V1 == input$ivariable)
      len <- length(index)
      if(len == 0){
        if(formulaValidate(formString) == "Error"){
          displayAlert("alert3")
        }
        else{
          insertIndepVar("insert")
        }
      }
      else {
        if(formulaValidate(formString) == "Error"){
          displayAlert("alert3")
        }
        else{
          insertIndepVar("update")
        }
      }
    }
    else{
      displayAlert("alert2")
    }
    updateTextInput(session,inputId = "iformula",value = "")
    updateTextInput(session, "ivariable", value = "")
    updateSelectInput(session = session, "icolumn",choices = colnames(MasterDF),selected = NULL) 
    
    toggleModal(session = session,modalId = "imodal", toggle = "close")
  })
  
  # observe the event while selecting the column to make formula in pop-up window.
  observeEvent(input$igoButton,{
    column <- unlist(isolate(input$icol))
    formString <<- input$iformula
    formString <<- paste(formString,column)
    indepList[[input$ivariable]][1] <- formString
    updateSelectInput(session = session, "icol",choices = colnames(MasterDF)[-1],selected = NULL)
    updateTextInput(session,inputId = "iformula",value = formString)
    
  })
  
  # function to insert the variable in indepTable using indepList.
  insertIndepVarWF <- function(action){
    indepList[[input$ivariable]] <<- c(input$ivariable,input$icolumn, input$iisFormula)
    indepTable <<- as.data.frame(t(as.data.frame.list(indepList)))
    temp <- with(MasterDF,eval(parse(text = as.character(input$icolumn))))
    
    displayindeptable(indepTable)
    if(action == "insert"){
      insertIndepTable(temp)
    }else if(action == "update"){
      updateIndepTable(temp)
    }
  }
  
  # function to insert the variable in indepTable using indepList.
  insertIndepVar <- function(action){
    indepListTemp <- as.list(NULL)
    indepTableTemp <- data.frame(NULL,stringsAsFactors = FALSE)
    indepListTemp[[input$ivariable]] <- c(input$ivariable,formString, input$iisFormula)
    indepTableTemp <- as.data.frame(t(as.data.frame.list(indepListTemp,stringsAsFactors = FALSE)))
    
    temp <- with(MasterDF,eval(parse(text = as.character(indepTableTemp[1,2]))))
    # Validate the new variable result, whether column with same result present in master table.
    matchCol <- columnCheck(MasterDF,temp)
    if(matchCol > 0){
      colPresentAlert(matchCol)
    }else{
      indepList[[input$ivariable]] <<- indepListTemp[[input$ivariable]]
      indepTable <<- as.data.frame(t(as.data.frame.list(indepList)))
      displayindeptable(indepTable)
      if(action == "insert"){
        insertIndepTable(temp)
      }else if(action == "update"){
        updateIndepTable(temp)
      }
    }
  }
  
  # Function to insert the independent variable as column into master table.
  insertIndepTable <- function(temp){
    MasterDF$temp <<- temp
    colnames(MasterDF)[which(names(MasterDF) == "temp")] <<- input$ivariable
    updateColumnDropDown(session)
    displayMasterTable(MasterDF)
  }
  
  # Function to update the independent column which is already present in the master table.
  updateIndepTable <- function(temp){
    MasterDF[,which(names(MasterDF) == input$ivariable)] <<- temp
    displayMasterTable(MasterDF)
  }
  
  # Function to delete the formula from independent variable and simultaneously updating the master table.
  observeEvent(input$idelete, {
    s = input$indeptable_rows_selected
    row_name <- rownames(indepTable)[s]
    if(!is.null(s)){
      delCol <- indepTable[s,1]
      indepList[row_name] <<- NULL
      indepTable <<- indepTable[-s,]
      if (length(s)) {
        displayAlert("alert6")
      }
      displayindeptable(indepTable)
      MasterDF <<- MasterDF[,!colnames(MasterDF) %in% row_name]
      updateColumnDropDown(session)
      displayMasterTable(MasterDF)
    }
  })
  
  # function for displaying indepTable of independent variable.
  displayindeptable <- function(indepTable){
    if(nrow(indepTable)==0){
      indepTable <- NULL
    }else {
      colnames(indepTable) <- c("Variable_Name", "Expersion/DB_Variable", "IsFromula")
      indepTable <- data.frame(indepTable)
    }
    output$indeptable <- DT::renderDataTable({
      indepTable
    },server = TRUE, rownames = FALSE)
  }
  
  
  #............................................................................
  # Common function which is used in above fucntions of indep and dep table.
  #............................................................................
  
  # function to validate formula using tryCatch().
  formulaValidate <- function(formula){
    tryCatch({
      parse(text = formula)
      paste("NoError")
    }, error = function(e){
      paste("Error")
    }
    )
  }
  
  # function for calling alert incase of various error.
  displayAlert <- function(alert){
    #alert incase variable name or column name is missing for dependent/independent variables
    if(alert == "alert1"){
      meCustomAlert(message = "Either Variable name is missing or column has not been selected.",alertType = "warning")
    }
    # alert if Variable name is missing where isFormula is true.
    else if(alert == "alert2"){
      meCustomAlert(message = "Variable name is missing.",alertType = "warning")
    }
    # alert if formula is syntatically wrong.
    else if(alert == "alert3"){
      meCustomAlert(message = "Formula is wrong.",alertType = "warning")
    }
    # alert if user wants to enter more than one dependent variable.
    else if(alert == "alert4"){
      meCustomAlert(message = "Only one dependent variable is allowed. You can modify the available dependent variable just by providing exact variable name in variable option",alertType = "warning")
    }
    # alert after droping any variable either from dependent variable.
    else if(alert == "alert5"){
      meCustomAlert(message = "Variable Droped. Just Now, You have droped dependent Variable.",alertType = "success")
    }
    # alert after droping any variable either from independent variable.
    else if (alert == "alert6"){
      meCustomAlert(message = "Variable Droped. Just Now, You have droped Independent Variable.",alertType = "success")
    }
    # alert to select column name in case user want to modify the dependent variable.
    else if (alert == "alert7"){
      meCustomAlert(message = "Missing Column input, Please select column for reseptive variable to modify.",alertType = "warning")
    }
  }
  
  # Function to check whether column is already present with same value in master table.
  columnCheck <- function(table, temp){
    check <- as.vector(sapply(table, function(x) identical(x,temp)))
    index <- which(check == TRUE)
    lenCheck <- length(index)
    if(lenCheck > 0) {
      return(index)
    } else {
      return(0)
    }
  }
  
  # function for calling alert incase of column with same result already present in master table while inserting a formula.
  colPresentAlert <- function(index){
    meCustomAlert(message = paste("Column '",colnames(MasterDF)[index],"' with same result is already present in MasterDF dataset."),alertType = "warning")
  }
  
  # function to download depVar dataset.
  output$download <- downloadHandler(
    filename = function() {
      paste("depTable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      colnames(depTable) <<- c("Variable_Name" , "Formula" , "Is_Formula")
      write.csv(depTable, file, row.names = FALSE)
    }
  )
  
  # function to download indepVar dataset.
  output$idownload <- downloadHandler(
    filename = function() {
      paste("indepTable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      colnames(indepTable) <<- c("Variable_Name" , "Formula" , "Is_Formula")
      write.csv(indepTable, file, row.names = FALSE)
    }
  )
  
  # function to download master dataset.
  output$mdownload <- downloadHandler(
    filename = function() {
      paste('MasterDF-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      downloadMasterDF <<- variableConsoleFile(MasterDF, "outputFile", varTag)
      write.csv(downloadMasterDF, file, row.names = FALSE)
    }
  )
  
  updateColumnDropDown <- function(session){
    updateSelectizeInput(session, inputId = "column", choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session, inputId = "icolumn", choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session, inputId = "col", choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session, inputId = "icol", choices = colnames(MasterDF)[-1], options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }')))
  }
  
  output$depInputDisplay <- renderUI({       
    fluidRow(
      column(4, textInput("variable","Variable Name", value = "")),
      column(3, selectizeInput('column', 'Column', choices = colnames(MasterDF), options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
      )),
      column(2, checkboxInput("isFormula","Is Formula", value = FALSE)),
      column(2, uiOutput("button_ui")),
      column(2, uiOutput("depPop"))
    )
  })
  
  output$inDepInputDisplay <- renderUI({
    fluidRow(
      column(4, textInput("ivariable","Variable Name", value = "")),
      column(3, selectizeInput('icolumn', 'Column', choices = colnames(MasterDF), options = list(placeholder = 'Please select a column', onInitialize = I('function() { this.setValue(""); }'))
      )),
      column(2, checkboxInput("iisFormula","Is Formula", value = FALSE)),
      column(2, uiOutput("ibutton_ui")),
      column(2, uiOutput("indepPop"))
    )
  })
  
  
  #############################################################################
  #####################   OLS manual process Acquire   ########################
  #############################################################################
  olsm_RegDataTemp <- NULL
  stackedModel <- NULL
  olsm_SplitByGeoList <- NULL
  olsm_parametersDF <- NULL
  olsmFinalTransRegDf <- NULL
  olsmFinalNormRegDf <- NULL
  olsmFinalRegDf <- NULL
  olsmformulaList <- NULL
  olsmAllModelList <- NULL
  olsmModelResult <- NULL
  olsm_values <- reactiveValues()
  olsmModelParameter <- NULL
  
  ##################### custom alert  #####################
  # meCustomAlert: function for custom alert message to user with alert type as error, success and warning.
  # This should always present in global.R file. It will return alert popup ui for Automodeller and OLSM and can be used in any module.
  meCustomAlert <- function(message, alertType){
    if(alertType=="error"){
      shinyalert(title = "",text = message,type = alertType,closeOnEsc = T,closeOnClickOutside = T,showConfirmButton = T,cancelButtonText = "Cancel",confirmButtonCol = "#AEDEF4",animation = T,imageUrl = "")
    }else if(alertType=="success"){
      shinyalert(title = "",text = message,type = alertType,closeOnEsc = T,closeOnClickOutside = T,showConfirmButton = T,cancelButtonText = "Cancel",confirmButtonCol = "#AEDEF4",animation = T,imageUrl = "")
    }else if(alertType=="warning"){
      shinyalert(title = "",text = message,type = alertType,closeOnEsc = T,closeOnClickOutside = T,showConfirmButton = T,cancelButtonText = "Cancel",confirmButtonCol = "#AEDEF4",animation = T,imageUrl = "")
    }
  }
  
  ##### OLSM Upload Screen
  output$olsm_upload <- renderUI({
    fluidRow(
      box(title = "Upload CSV File",status = "success",solidHeader = T,width = 6,
          fileInput('olsm_file',label = NULL,accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
          fluidRow(
            column(4, actionButton("olsm_uploadCsv",label = "Continue",icon = icon("upload"),style="color: #212121; background-color: #c8e6c9; border-color: #212121; float: left"))
          )
      ),
      uiOutput("olsm_uploadContents")
    )
    
  })
  
  observeEvent(input$olsm_uploadCsv,{
    withProgress(message = 'uploading File ....', value = 0.85, {
      olsm_file <- input$olsm_file
      if (is.null(olsm_file)){
        meCustomAlert(message = "Data missing, Please upload the data.",alertType = "warning")
      } else {
        tryCatch({
          olsm_RegDataTemp <<- read.csv(olsm_file$datapath,header = TRUE,stringsAsFactors = FALSE,check.names = FALSE)
          stackedModel <<- FALSE
          names(olsm_RegDataTemp) <- make.names(names(olsm_RegDataTemp),unique = TRUE)
          
          if(any(names(olsm_RegDataTemp) %in% "Geography")){
            stackedModel <<- TRUE
            olsm_RegDataTemp$Geography <- as.factor(olsm_RegDataTemp$Geography)
            olsm_SplitByGeoList <<- split(olsm_RegDataTemp, olsm_RegDataTemp$Geography)
          }
          
          # Changing Intercept variable name to DummyIntrcpt to avoid grep clash for Intercept in Model Result building.
          if(any(grepl("Intercept", names(olsm_RegDataTemp)))){
            names(olsm_RegDataTemp)[grep("Intercept", names(olsm_RegDataTemp))] <<- "DummyIntrcpt"
          }
          
          olsmDisplayUploadedData()
          
        }, error = function(err){
          meCustomAlert(message = "Error in Uploaded file, Please check the column names in the uploaded file. Special Characters may present.",alertType = "error")
        })
      }
    })
  })
  
  olsmDisplayUploadedData <- function(){
    
    output$olsm_uploadContents <- renderUI({
      tagList(
        uiOutput('olsm_dataScope'),
        box(title = "Uploaded Data",status = "success",width = "12",solidHeader = T,collapsible = F,
            DT::dataTableOutput('olsm_contents')
        )
      )
    })
    
    output$olsm_contents <- renderDataTable(datatable({
      olsm_RegDataTemp
    },rownames = FALSE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      paging = TRUE,scrollX = TRUE,scrollY = TRUE,searching = FALSE,autoWidth = TRUE,
      columnDefs = list(list(width = '120px', targets = "_all"))
    ), selection = 'none')
    )
    
    Period <- lubridate::dmy(olsm_RegDataTemp$Period)
    if(is.null(input$olsm_startDate) & is.null(input$olsm_endDate)){
      start_date <- min(Period)
      end_date <- max(Period)
    }else if(input$olsm_startDate == min(Period) & input$olsm_endDate == max(Period)){
      start_date <- min(Period)
      end_date <- max(Period)
    }else {
      start_date <- input$olsm_startDate
      end_date <- input$olsm_endDate
    }
    
    
    output$olsm_dataScope <- renderUI({
      box(title = "Select Data scope (AF Scope)",status = "warning",width = 6,solidHeader = T,
          dateInput('olsm_startDate', label = "Data start Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = start_date,min = min(Period),max = max(Period)),
          dateInput('olsm_endDate', label = "Data end Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = end_date,min = min(Period),max = max(Period)),
          actionButton("olsmNavToMM","Click for Model Manager", style="color: #212121; background-color: #fbc998; border-color: #212121; float: left")
      )
    })
  }
  
  observeEvent(input$olsmNavToMM,{
    if(any(names(olsm_RegDataTemp) %in% "period")){
      names(olsm_RegDataTemp)[which(names(olsm_RegDataTemp) %in% "period")] <- "Period"
    }
    
    if(length(input$olsm_startDate)!= 0 & length(input$olsm_endDate) != 0){
      olsm_RegDataTemp <<- subset(olsm_RegDataTemp, dmy(olsm_RegDataTemp$Period) >= input$olsm_startDate & dmy(olsm_RegDataTemp$Period) <= input$olsm_endDate)
      updateTabItems(session, "sidebartabs", "olsm_modelScope")
      
      if(stackedModel == TRUE & length(unique(as.factor(olsm_RegDataTemp$Geography))) > 1){
        olsmSelectGeoUI()
      }else{
        vars <- colnames(olsm_RegDataTemp[,-which(names(olsm_RegDataTemp) %in% c("Period","Geography"))])
        olsmSelectVarUI()
      }
    }else{
      meCustomAlert(message = "Date Format Error!, Please check the date.",alertType = "error")
    }
  })
  
  ################################# Geography Selection ######################
  
  olsmSelectGeoUI <- function(){
    output$olsm_getGeography <- renderUI({
      fluidRow(
        box(title = "SELECT GEOGRAPHY",status = "danger",width = 12,collapsible = T,solidHeader = T,
            fluidRow(
              box(title = "Select Geography",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsmSelectallGeo",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmGeography_data")
              ),
              column(width = 2,
                     box(title = "Add",background = "purple",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmAddGeography",label = ">>",width = "100%")
                     ),
                     box(title = "Remove",background = "maroon",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmDeleteGeography",label = "<<",width = "100%")
                     ),
                     box(width = NULL,solidHeader = TRUE,status = "success",
                         actionButton(inputId = "olsmUpdateGeography",label = "Continue",width = "100%",style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left")
                     )
              ),
              box(title = "Selected Geography",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsmUnselectallGeo",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmGeography_columns")
              )
            )
        )
      )
    })
    
  }
  
  output$olsmGeography_data <- renderRHandsontable({
    if (!is.null(input$olsmGeography_data)) {
      geographyData <- hot_to_r(input$olsmGeography_data)
    } else {
      geographyData <- data.frame(Geography = unique(olsm_RegDataTemp$Geography),SelectGeography = rep(FALSE, length(unique(olsm_RegDataTemp$Geography))),stringsAsFactors = FALSE)
    }
    rhandsontable(geographyData,
                  useTypes = TRUE, stretchH = "all", selectCallback = T,
                  fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  ####### SelectAll or UnselectAll Geography in Geography_data UI #####
  observeEvent(input$olsmSelectallGeo,{
    GeographyALLtest <- hot_to_r(input$olsmGeography_data)
    if(all(GeographyALLtest$SelectGeography == TRUE)){
      GeographyALLtest[,"SelectGeography"] <- FALSE
      output$olsmGeography_data <- renderRHandsontable({
          rhandsontable(GeographyALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsmSelectallGeo",label = "select All")
    }else{
      GeographyALLtest[,"SelectGeography"] <- TRUE
      output$olsmGeography_data <- renderRHandsontable({
          rhandsontable(GeographyALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsmSelectallGeo",label = "Unselect All")
    }
    
  })
  
  ####### SelectAll or UnselectAll Geography in Geography_columns UI #####
  observeEvent(input$olsmUnselectallGeo,{
    if(!is.null(input$olsmGeography_columns)){
      GeographyALLtest <- hot_to_r(input$olsmGeography_columns)
      if(all(GeographyALLtest$SelectGeography == TRUE)){
        GeographyALLtest[,"SelectGeography"] <- FALSE
        output$olsmGeography_columns <- renderRHandsontable({
          rhandsontable(GeographyALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsmUnselectallGeo",label = "select All")
      }else{
        GeographyALLtest[,"SelectGeography"] <- TRUE
        output$olsmGeography_columns <- renderRHandsontable({
          rhandsontable(GeographyALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsmUnselectallGeo",label = "Unselect All")
      }
    }
  })
  
  ##### add Geography #######
  observeEvent( input$olsmAddGeography,{
    Geographytest <- hot_to_r(input$olsmGeography_data)
    if(any(Geographytest$SelectGeography == TRUE)){
      Geographytest <- data.frame(Geographytest[which(Geographytest$SelectGeography==TRUE),])
      Geographytest[which(Geographytest$SelectGeography== TRUE),"SelectGeography"] <- FALSE
      
      output$olsmGeography_columns <- renderRHandsontable({
        rhandsontable(Geographytest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(Geographytest) != 0){
        updateActionButton(session,inputId = "olsmUnselectallGeo",label = "select All")
        shinyjs::show(id = "olsmUnselectallGeo")
      }
    }
  })
  
  ####### delete Geography ######                      
  observeEvent(input$olsmDeleteGeography,{
    SelectedGeoCol <- hot_to_r(input$olsmGeography_columns)
    if(any(SelectedGeoCol$SelectGeography == TRUE)){
      SelectedGeoCol <- SelectedGeoCol[-which(SelectedGeoCol$SelectGeography == TRUE),]
      
      output$olsmGeography_columns = renderRHandsontable({
        rhandsontable(SelectedGeoCol,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(SelectedGeoCol) == 0){
        shinyjs::hide(id = "olsmUnselectallGeo")
      }
    }
  })
  
  ####### Save Selected Geography and open Variable selection panel ########
  observeEvent(input$olsmUpdateGeography,{
    if(length(input$olsmGeography_columns$data)!=0){
      olsm_SelectedGeos <- hot_to_r(input$olsmGeography_columns)
      olsmModelParameter[["olsm_SelectedGeos"]] <<- olsm_SelectedGeos$Geography
      olsmSelectVarUI()
    }else{
      meCustomAlert(message = "Please select atleast one geography.",alertType = "warning")
    }
    
  })
  
  ################################# Variable Selection ######################
  
  olsmSelectVarUI <- function(){
    output$olsm_getModelVar <- renderUI({
      fluidRow(
        box(title = "SELECT VARIABLES",status = "danger",width = 12,collapsible = T,solidHeader = T,
            fluidRow(
              box(title = "Select Variables",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsm_SelectedVars",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmVariables")
              ),
              column(width = 2,
                     box(title = "Add",background = "purple",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmAddVariables",label = ">>",width = "100%")
                     ),
                     box(title = "Remove",background = "maroon",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmDeleteVariables",label = "<<",width = "100%")
                     ),
                     box(width = NULL,solidHeader = TRUE,status = "success",
                         actionButton(inputId = "olsmUpdateVariable",label = "Continue",width = "100%",style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left")
                     ),
                     box(width = NULL,solidHeader = TRUE,status = "success",
                         actionButton(inputId = "olsm_backcsvUp",label = "Back",width = "100%",style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left")
                     )
              ),
              box(title = "Selected Variables",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsm_UnselectedVars",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmVariables_column")
              )
            )
        )
      )
    })
  }
  
  output$olsmVariables <- renderRHandsontable({
    if (!is.null(input$olsmVariables)) {
      VariablesData <- hot_to_r(input$olsmVariables)
    } else {
      var <- names(olsm_RegDataTemp)[!names(olsm_RegDataTemp) %in% c("Geography", "Period")]
      VariablesData <- data.frame(Variables = var,SelectVariable = rep(FALSE, length(var)),stringsAsFactors = FALSE)
    }
    rhandsontable(VariablesData,
                  useTypes = TRUE, stretchH = "all", selectCallback = T,
                  fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  ####### SelectAll or UnselectAll variable in olsmVariables UI #####
  observeEvent(input$olsm_SelectedVars,{
    VariableALLtest <- hot_to_r(input$olsmVariables)
    if(all(VariableALLtest$SelectVariable == TRUE)){
      VariableALLtest[,"SelectVariable"] <- FALSE
      output$olsmVariables <- renderRHandsontable({
        rhandsontable(VariableALLtest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsm_SelectedVars",label = "select All")
    }else{
      VariableALLtest[,"SelectVariable"] <- TRUE
      output$olsmVariables <- renderRHandsontable({
        rhandsontable(VariableALLtest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsm_SelectedVars",label = "Unselect All")
    }
  })
  
  ####### SelectAll or UnselectAll Geography in Geography_columns UI #####
  observeEvent(input$olsm_UnselectedVars,{
    if(!is.null(input$olsmVariables_column)){
      VariableALLtest <- hot_to_r(input$olsmVariables_column)
      if(all(VariableALLtest$SelectVariable == TRUE)){
        VariableALLtest[,"SelectVariable"] <- FALSE
        output$olsmVariables_column <- renderRHandsontable({
          rhandsontable(VariableALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsm_UnselectedVars",label = "select All")
      }else{
        VariableALLtest[,"SelectVariable"] <- TRUE
        output$olsmVariables_column <- renderRHandsontable({
          rhandsontable(VariableALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsm_UnselectedVars",label = "Unselect All")
      }
    }
  })
  
  ################ ADD Variables #########
  observeEvent( input$olsmAddVariables,{
    Variablestest <- hot_to_r(input$olsmVariables)
    if(any(Variablestest$SelectVariable == TRUE)){
      VariablesData <- data.frame(Variablestest[which(Variablestest$SelectVariable==TRUE),])
      VariablesData[which(VariablesData$SelectVariable == TRUE),"SelectVariable"] <- FALSE
      output$olsmVariables_column <- renderRHandsontable({
        rhandsontable(VariablesData,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(Variablestest) != 0){
        updateActionButton(session,inputId = "olsm_UnselectedVars",label = "select All")
        shinyjs::show(id = "olsm_UnselectedVars")
      }
    }
  })
  
  ################ Remove Variables ######
  observeEvent(input$olsmDeleteVariables,{
    SelectedVar <- hot_to_r(input$olsmVariables_column)
    if(any(SelectedVar$SelectVariable == TRUE)){
      SelectedVar <- SelectedVar[-which(SelectedVar$SelectVariable == TRUE),]
      output$olsmVariables_column = renderRHandsontable({
        rhandsontable(SelectedVar,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(SelectedVar) == 0){
        shinyjs::hide(id = "olsm_UnselectedVars")
      }
    }
  })
  
  ####### Save Selected Variable and open period selection panel ########
  observeEvent(input$olsmUpdateVariable,{
    if(length(input$olsmVariables_column$data) > 1){
      olsm_SelectedVar <- hot_to_r(input$olsmVariables_column)
      olsmModelParameter[["olsm_SelectedVar"]] <<- olsm_SelectedVar$Variables
      olsmSelectPeiordUI()
    }else{
      meCustomAlert(message = "Please select atleast two variables (one target and one independent variable).",alertType = "warning")
      
    }
    
  })
  
  # navigate to upload csv page from variable selection
  observeEvent(input$olsm_backcsvUp,{
    updateTabItems(session, "sidebartabs", "olsm_csvUp")
  })
  
  ################################# Period Selection ######################
  
  olsmSelectPeiordUI <- function(){
    output$olsm_modellingScope <- renderUI({
      fluidRow(
        box(title = "SELECT PERIOD",status = "danger",width = 12,collapsible = T,solidHeader = T,
            fixedRow(
              box(title = "Select Period",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsmSelectallPeriod",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmPeriod_data")
              ),
              column(width = 2,
                     box(title = "Add",background = "purple",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmAddPeriod",label = ">>",width = "100%")
                     ),
                     box(title = "Remove",background = "maroon",width = NULL,solidHeader = TRUE,
                         actionButton(inputId = "olsmDeletePeriod",label = "<<",width = "100%")
                     ),
                     box(width = NULL,solidHeader = TRUE,status = "success",
                         actionButton(inputId = "olsmUpdatePeriod",label = "Continue",width = "100%",style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left")
                     )
              ),
              box(title = "Selected Period",status = "info",skin = "blue",width = 5,solidHeader = T,
                  actionButton(inputId = "olsm_UnselectedPeriod",label = "Select All",icon = icon("list-alt"),style="color: #ffffff; background-color: #33cccc; border-color: #ffffff; float:left"),br(),br(),
                  rHandsontableOutput("olsmPeriod_columns")
              )
            )
        )
      )
    })
  }
  
  output$olsmPeriod_data <- renderRHandsontable({
    if (!is.null(input$olsmPeriod_data)){
      PeriodData = hot_to_r(input$olsmPeriod_data)
    } else {
      PeriodData <- data.frame(Data_Period = unique(olsm_RegDataTemp$Period), SelectPeriod = rep(FALSE, length(unique(olsm_RegDataTemp$Period))),stringsAsFactors = FALSE)
    }
    rhandsontable(PeriodData,
                  useTypes = TRUE, stretchH = "all", selectCallback = T,
                  fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  ####### SelectAll or UnselectAll Period in olsmPeriod_data UI #####
  observeEvent(input$olsmSelectallPeriod,{
    PeriodALLtest <- hot_to_r(input$olsmPeriod_data)
    if(all(PeriodALLtest$SelectPeriod == TRUE)){
      PeriodALLtest[,"SelectPeriod"] <- FALSE
      output$olsmPeriod_data <- renderRHandsontable({
        rhandsontable(PeriodALLtest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsmSelectallPeriod",label = "select All")
    }else{
      PeriodALLtest[,"SelectPeriod"] <- TRUE
      output$olsmPeriod_data <- renderRHandsontable({
        rhandsontable(PeriodALLtest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateActionButton(session,inputId = "olsmSelectallPeriod",label = "unselect All")
    }
    
  })
  
  ####### SelectAll or UnselectAll Period in olsmPeriod_data UI #####
  observeEvent(input$olsm_UnselectedPeriod,{
    if(!is.null(input$olsmPeriod_columns)){
      PeriodALLtest <- hot_to_r(input$olsmPeriod_columns)
      if(all(PeriodALLtest$SelectPeriod == TRUE)){
        PeriodALLtest[,"SelectPeriod"] <- FALSE
        output$olsmPeriod_columns <- renderRHandsontable({
          rhandsontable(PeriodALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsm_UnselectedPeriod",label = "select All")
      }else{
        PeriodALLtest[,"SelectPeriod"] <- TRUE
        output$olsmPeriod_columns <- renderRHandsontable({
          rhandsontable(PeriodALLtest,
                        useTypes = TRUE, stretchH = "all", selectCallback = T,
                        fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        updateActionButton(session,inputId = "olsm_UnselectedPeriod",label = "Unselect All")
      }
    }
  })
  
  ####### Add Period ############
  observeEvent( input$olsmAddPeriod,{
    Periodtest <- hot_to_r(input$olsmPeriod_data)
    if(any(Periodtest$SelectPeriod == TRUE)){
      Periodtest <- data.frame(Periodtest[which(Periodtest$SelectPeriod==TRUE),])
      Periodtest[which(Periodtest$SelectPeriod== TRUE),"SelectPeriod"] <- FALSE
      output$olsmPeriod_columns <- renderRHandsontable({
        rhandsontable(Periodtest,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(Periodtest) != 0){
        updateActionButton(session,inputId = "olsm_UnselectedPeriod",label = "select All")
        shinyjs::show(id = "olsm_UnselectedPeriod")
      }
    }
  })
  
  ############## Remove Period ########
  observeEvent(input$olsmDeletePeriod,{
    SelectedPeriodCol <- hot_to_r(input$olsmPeriod_columns)
    if(any(SelectedPeriodCol$SelectPeriod == TRUE)){
      SelectedPeriodCol <- SelectedPeriodCol[-which(SelectedPeriodCol$SelectPeriod == TRUE),]
      output$olsmPeriod_columns = renderRHandsontable({
        rhandsontable(SelectedPeriodCol,
                      useTypes = TRUE, stretchH = "all", selectCallback = T,
                      fillHandle = list(direction='vertical', autoInsertRow=FALSE)) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      
      if(nrow(SelectedPeriodCol) == 0){
        shinyjs::hide(id = "olsm_UnselectedPeriod")
      }
    }
  })
  
  ####### Save Selected Perios and open Model Manager Tab ########
  observeEvent(input$olsmUpdatePeriod,{
    if(length(input$olsmPeriod_columns$data) > 1){
      olsm_SelectedPeriod <- hot_to_r(input$olsmPeriod_columns)
      olsmModelParameter[["olsm_SelectedPeriod"]] <<- olsm_SelectedPeriod$Data_Period
      olsmGetModelManagerUI()
    }else {
      meCustomAlert(message = "Please select atleast two dates.",alertType = "warning")
    }
  })
  
  ################################# Model Manager ######################
  olsmGetModelManagerUI <- function(){
    if(is.null(olsm_parametersDF)){
      output$olsm_displayModelManager <- renderUI({
        fluidRow(
          box(title = "Model Manager",status = "success",width = "12",solidHeader = T,collapsible = F,
              wellPanel(
                fluidRow(
                  column(3,radioButtons(inputId = "olsmDecayPos",label = "Select Adstock Type",choices = c("AdStock Last", "AdStock First"),selected = NULL, inline = TRUE)),
                  column(3,radioButtons(inputId = "olsmHasIntercept",label = "Has Intercept",choices = c("Yes", "No"),selected = NULL, inline = TRUE)),
                  column(3,radioButtons(inputId = "olsmWLSChoice",label = "WLS",choices = c("No", "Yes"),selected = NULL, inline = TRUE)),
                  column(3,radioButtons(inputId = "olsmMixedModelChoice",label = "Mixed Model",choices = c("No", "Yes"),selected = NULL, inline = TRUE))
                )
              ),
              actionButton("olsm_ResetModelManager","Reset",icon = icon("refresh"),style="color: #ffffff; background-color: #33cccc ; border-color: #ffffff; float: right"),
              actionButton("olsm_RefreshModelManager","Refresh Model Manager",icon = icon("refresh"),style="color: #ffffff; background-color: #33cccc ; border-color: #ffffff; float: right"),
              br(),
              rHandsontableOutput("olsm_mmTable"),
              br(),
              fluidRow(
                column(2, actionButton("olsm_runRegression","Run Regression",style="color: #ffffff; background-color: #ec407a ; border-color: #2e6da4")),
                column(3,downloadButton("olsm_downloadModelManager","Download Model Manager",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4")),
                column(2, actionButton("olsm_backmodelScope","Back",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4"))
              )
          )
        )
      })
      
      olsm_parametersDF <<-  olsm_createModelManagerData(olsmModelParameter$olsm_SelectedVar)
      olsm_parametersDF <<- data.frame(olsm_parametersDF,row.names = seq(1:nrow(olsm_parametersDF)))
      olsmDisplayModelManager(olsm_parametersDF)
    }
    updateTabItems(session, "sidebartabs", "olsm_modelManager")
  }
  
  olsmDisplayModelManager <- function(olsm_parametersDF){
    output$olsm_mmTable <- renderRHandsontable({
      rhandsontable(olsm_parametersDF,
                    useTypes = TRUE, stretchH = "all", selectCallback = T,
                    fillHandle = list(direction='vertical', autoInsertRow=FALSE), 
                    maxRows = nrow(olsm_parametersDF)) %>%
        hot_col(c("VariableName"), readOnly = TRUE)%>%
        hot_rows(fixedRowsTop = 1) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>% 
        hot_cols(fixedColumnsLeft = 1, renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col == 0) {
                 td.style.background = '#fffdd0';  
                 }
                 else if(col == 1 ){
                 td.style.background = '#b3d9ff';
                 }
                 else if(instance.getData()[row][1] == 'Not in Model'){
                 if( col != 1){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 }
                 else if(instance.getData()[row][1] == 'DepVar'){
                 if( col != 1 && col != 15){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else {cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Manual No Trans' | instance.getData()[row][1] == 'Outside No Trans'){
                 if(col != 1  && col!= 15 && col!= 16 && col!= 18 && col!= 19 ){ 
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else  { 
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66'
                 }
                 }
                 else if(instance.getData()[row][1] == 'Fixed Var No Trans'){
                 if(  col!= 1  && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 17 ) {
                 cellProperties.readOnly = true;
                 
                 td.style.background = '#dddddd';
                 } else {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Manual TOF' && instance.getData()[row][2] == 'Linear'
                 | instance.getData()[row][2] == 'S-Shaped_New'){
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19   ){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else { cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Manual TOF' && instance.getData()[row][2] == 'Decay'){
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 ){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Manual TOF' && instance.getData()[row][2] == 'Power'){
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 && col!= 9){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd'; 
                 }
                 else {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Manual TOF' && instance.getData()[row][2] == 'S-Shaped' |
                 instance.getData()[row][2] == 'S-Shaped_Decay_Capped' ) {
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 && col!= 9 && col!= 11 ){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd'; 
                 }
                 else {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66' 
                 }
                 }
                 else if(instance.getData()[row][1] == 'Fixed Var TOF' && instance.getData()[row][2] == 'Linear' |instance.getData()[row][2] == 'S-Shaped_New'){
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 17){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else{cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Fixed Var TOF' && instance.getData()[row][2] == 'Power'){
                 if( col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 && col!= 9 && col!=17){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Fixed Var TOF' && instance.getData()[row][2] == 'Decay') {
                 if(col !=1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 && col!= 6 && col!= 17){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else{cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Fixed Var TOF' && instance.getData()[row][2] == 'S-Shaped' |instance.getData()[row][2] == 'S-Shaped_Decay_Capped' ) {
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 
                 && col!= 9 && col!= 11 && col != 17 ){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd'; 
                 }
                 else {cellProperties.readOnly = false;
                 td.style.background = '#b3ff66' 
                 }
                 }
                 else if(instance.getData()[row][1] == 'Outside TOF' && instance.getData()[row][2] == 'Linear'
                 | instance.getData()[row][2] == 'S-Shaped_New'){
                 if(col != 1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 ){
                 cellProperties.readOnly = true;  
                 td.style.background = '#dddddd';
                 }
                 else { cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Outside TOF' && instance.getData()[row][2] == 'Decay'){
                 if(col !=1 && col != 2 && col!= 15 && col!= 16 && col!= 18 && col!= 19 && col!= 3 && col!= 6 && col!= 6 && col!= 4 && col!= 5 && col!= 7){
                 cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 else { cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Outside TOF' && instance.getData()[row][2] == 'Power'){
                 if(col !=0 && col !=17 && col != 11 && col!= 12 && col!= 13 && col!= 14  ){
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 else { cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }
                 }
                 else if(instance.getData()[row][1] == 'Outside TOF' && instance.getData()[row][2] == 'S-Shaped' | instance.getData()[row][2] == 'S-Shaped_Decay_Capped' ){
                 if(col!=0 && col != 17)
                 {
                 cellProperties.readOnly = false;
                 td.style.background = '#b3ff66';
                 }
                 else { cellProperties.readOnly = true;
                 td.style.background = '#dddddd';
                 }}
                 
                 
                 }
                 ")
    })
  }
  
  observeEvent(input$olsm_RefreshModelManager,{
    olsm_SelectedVars <- olsmModelParameter$olsm_SelectedVar
    olsm_removeVar <- setdiff(olsm_parametersDF$VariableName, olsm_SelectedVars)
    olsm_addVar <- setdiff(olsm_SelectedVars,olsm_parametersDF$VariableName)
    DF <- hot_to_r(input$olsm_mmTable)
    if(length(olsm_removeVar)!=0){
      DF <- data.frame(DF[-which(DF$VariableName %in% olsm_removeVar),],row.names = NULL)
    }
    if(length(olsm_addVar)!= 0){
      DF <- data.frame(rbind(DF,olsm_createModelManagerData(olsm_addVar)),row.names = NULL)
    }
    olsm_parametersDF <<- DF
    olsmDisplayModelManager(olsm_parametersDF)
    meCustomAlert(message = "Model Manager is updated",alertType = "success")
  })
  
  observeEvent(input$olsm_ResetModelManager,{
    olsm_parametersDF <<-  olsm_createModelManagerData(olsmModelParameter$olsm_SelectedVar)
    olsm_parametersDF <<- data.frame(olsm_parametersDF,row.names = seq(1:nrow(olsm_parametersDF)))
    olsmDisplayModelManager(olsm_parametersDF)
    meCustomAlert(message = "Model Manager is reseted",alertType = "success")
  })
  
  observeEvent(input$olsm_backmodelScope,{
    updateTabItems(session, "sidebartabs", "olsm_modelScope")
  })
  
  output$olsm_downloadModelManager <- downloadHandler(
    filename = function() {
      "ModelManager.csv"
    },
    content = function(file) {
      write.csv(hot_to_r(input$olsm_mmTable), file, row.names = FALSE)
    }
  )
  
  ##### OLSM Model Result
  olsmAllModelList <<- list()
  olsmModelResult <<- data.frame()
  olsmDummyModelsList <<- list()
  
  observeEvent(input$olsm_runRegression,{
    olsm_parametersDF <<- hot_to_r(input$olsm_mmTable)
    nonNumParameters <- names(olsm_parametersDF)[!sapply(olsm_parametersDF, is.numeric)]
    tryCatch({
      
      # Condition 1: OLSM: Dependent should be selected.
      if(any(olsm_parametersDF$Type %in% "DepVar") & (sum(olsm_parametersDF$Type %in% "DepVar")==1)){
        
        # Condition 2: OLSM: No Negative Values or Missing Values in Model Manager.
        if(!any(sapply(olsm_parametersDF[,!names(olsm_parametersDF) %in% c(nonNumParameters,"Fixed_Coefficient")], function(x) sign(x)== -1),na.rm = TRUE) & !any(is.na(sapply(olsm_parametersDF[,!names(olsm_parametersDF) %in% nonNumParameters], function(x) sign(x)== -1)))){
          
          # Condition 3: OLSM: Combined Column option should not be selected for Outside TOF, Outside No Trans and DepVar.
          if(all(olsm_parametersDF[which(olsm_parametersDF$Type %in% c("Outside TOF","Outside No Trans","DepVar")),"Combined_Column"]==0)){
            
            combinedColumns <- olsm_parametersDF[which(olsm_parametersDF$Combined_Column != 0),c("VariableName","Combined_Column")]
            singleCombinedColumn <- plyr::count(as.factor(combinedColumns$Combined_Column))
            
            # Condition 4: OLSM: Combined Column value should be present in atleast for two variable except depvar.
            if(all(singleCombinedColumn$freq !=1)){
              
              modelFeatureList <- NULL
              
              modelFeatureList <- list(input$olsm_startDate,input$olsm_endDate,olsmModelParameter$olsm_SelectedPeriod,input$olsmDecayPos,input$olsmHasIntercept,input$olsmMixedModelChoice, input$olsmWLSChoice)
              names(modelFeatureList) <- c("startDate","endDate", "modellingPeriod", "adStockChoice", "hasIntercept","mixedModelChioce","wLSChoice") 
              olsmResult <- NULL
              
              if(stackedModel==FALSE){
                if(input$olsmWLSChoice == "No"){
                  if(input$olsmMixedModelChoice == "No"){
                    # Non stacked OLS Model
                    withProgress(message = 'Modelling is inprogress.', detail = 'This may take a while..., please be patient.', value = 0.85, {
                      olsmResult <- olsmGetNonStackedOLSModel(olsm_RegDataTemp,olsm_parametersDF,modelFeatureList)
                    })
                  }else if(input$olsmMixedModelChoice == "Yes"){
                    # Condition 6: Random Effect Modelling is not applicable for Non Stacked Model.
                    meCustomAlert(message = "Mixed Effect Modelling is not applicable for Non Stacked Model. Geography Must be present.",alertType = "error")
                  }
                }else if(input$olsmWLSChoice == "Yes"){
                  # Condition 5: WLS is not applicable for Non Stacked Model.
                  meCustomAlert(message = "WLS(weighted) Regression is not applicable for Non Stacked Model. Geography Must be present.",alertType = "error")
                }
              }else if(stackedModel==TRUE){
                
                withProgress(message = 'Modelling is inprogress.', detail = 'This may take a while..., please be patient.', value = 0.85, {
                  
                  modelFeatureList[["selectedGeos"]] <- olsmModelParameter$olsm_SelectedGeos
                  if(input$olsmWLSChoice == "No"){
                    if(input$olsmMixedModelChoice == "No"){
                      # Stacked OLS Model
                      olsmResult <-  olsmGetStackedOLSModel(olsm_RegDataTemp,olsm_parametersDF,olsm_SplitByGeoList,modelFeatureList,type = "OLS")
                    }else if(input$olsmMixedModelChoice == "Yes"){
                      # Condition 7: Random Effect: Outside iteration is not allowed for Mixed Modelling.
                      if(!any(olsm_parametersDF$Type %in% c("Outside TOF","Outside No Trans"))){
                        # Condition 8: Random Effect: Check for random effect variable for mixed modelling except Depvar
                        if(any(olsm_parametersDF$Random_Effect != 0 & olsm_parametersDF$Type !="DepVar")){
                          # Stacked Mixed Model without weight.
                          tryCatch({
                            olsmResult <- olsmGetStackedOLSModel(olsm_RegDataTemp,olsm_parametersDF,olsm_SplitByGeoList,modelFeatureList,type = "Mixed")
                          }, error = function(err){
                            meCustomAlert(message = "Model didn't converge.",alertType = "warning")
                          })
                        }else{
                          meCustomAlert(message = "Random Effect Variable is not selected.",alertType = "error")
                        }
                      }else{
                        meCustomAlert(message = "Outside iteration is not allowed for Mixed Modelling.",alertType = "error")
                      }
                    }
                  }else if(input$olsmWLSChoice == "Yes"){
                    # Mixed Model should not be selected for WLS.
                    if(input$olsmMixedModelChoice=="No"){
                      olsmResult <-  olsmGetStackedOLSModel(olsm_RegDataTemp,olsm_parametersDF,olsm_SplitByGeoList,modelFeatureList,type = "WLS")
                    }else if(input$olsmMixedModelChoice=="Yes"){
                      # Condition 7: Random Effect: Outside iteration is not allowed for Mixed Modelling.
                      if(!any(olsm_parametersDF$Type %in% c("Outside TOF","Outside No Trans"))){
                        # Condition 8: Random Effect: Check for random effect variable for mixed modelling except Depvar
                        if(any(olsm_parametersDF$Random_Effect != 0 & olsm_parametersDF$Type !="DepVar")){
                          # Stacked Mixed Model with weight.
                          tryCatch({
                            olsmResult <- olsmGetStackedOLSModel(olsm_RegDataTemp,olsm_parametersDF,olsm_SplitByGeoList,modelFeatureList, type = "Mixed")
                          }, error = function(err){
                            meCustomAlert(message = "Model didn't converge.",alertType = "warning")
                          })
                        }else{
                          meCustomAlert(message = "Random Effect Variable is not selected.",alertType = "error")
                        }
                      }else{
                        meCustomAlert(message = "Outside iteration is not allowed for Mixed Modelling.",alertType = "error")
                      }
                    }
                  }
                })
              }
              
              if(!is.null(olsmResult)){
                olsmFinalTransRegDf <<- olsmResult[["olsmFinalTransRegDf"]]
                olsmFinalNormRegDf <<- olsmResult[["olsmFinalNormRegDf"]]
                olsmFinalRegDf <<- olsmResult[["olsmFinalRegDf"]]
                olsmAllModelList <<- olsmResult[["olsmAllModelList"]]
                olsmformulaList <<- olsmResult[["formulaList"]]
                
                # Section for Publishing Model Result for OLS/WLS/Mixed Model.
                if(input$olsmMixedModelChoice == "No"){
                  
                  olsmModelResult <<- olsmResult[["olsmModelResult"]] 
                  # allModelList will have total number of dummy model generated through main model.
                  olsmDummyModelsList <<- as.list(rep(0,length(olsmAllModelList)))
                  
                  # calling function to display UI of olsm Model Result Table.
                  if(stackedModel==FALSE){
                    olsm_displayUIResultTable(stacked = FALSE,splitVar = olsm_splitByGeoSubset)
                  }else if(stackedModel==TRUE){
                    olsm_displayUIResultTable(stacked = TRUE,splitVar = olsm_splitByGeoSubset)
                  }
                  updateTabItems(session, "sidebartabs", "olsm_results")
                  
                } else if(input$olsmMixedModelChoice == "Yes"){
                  # calling function to display download option for mixed model result.
                  olsm_displayUIResultTable(stacked = TRUE,splitVar = olsm_splitByGeoSubset)
                  updateTabItems(session, "sidebartabs", "olsm_results")
                }
              }
              
            }else{
              meCustomAlert(message = paste0("Combined Column Feature Error, Invalid single Combined_Column value Entry for ",toString(singleCombinedColumn$x[singleCombinedColumn$freq==1]), ". Please check the Model Manager."),alertType = "error")
            }
          }else{
            meCustomAlert(message = "Combined column option given for Outside or Dependent Variable (which is not applicable).",alertType = "error")
          }
        }else{
          meCustomAlert(message = "Please check for negative or missing values in model manager.",alertType = "error")
        }
      }else{
        meCustomAlert(message = "Please select one dependent variable for modelling.",alertType = "error")
      }
    }, error = function(err){
      meCustomAlert(message = "Error in Modelling!, Please check the dataset. (Possible Issue: Column Name Format(Special Character), Data Format(Non Numeric Values), Negative value in data after processing etc.)",alertType = "error")
    })
  })
  
  output$olsmDisplayResult <- renderUI({
    if(input$olsmMixedModelChoice == "No"){
      fluidRow(
        box(title = "Model Result Details", status = "success", width = "12", solidHeader = T, collapsible = F,
            fluidRow(
              column(12, dataTableOutput("olsm_resultTable"))
            ),
            fluidRow(
              column(2, downloadButton("olsmDownLoadModelResult", "Download Model",style="color: #212121; background-color: #ffc8b3; border-color: #212121; float:left")),
              column(3, downloadButton("olsmDownLoadConsolidateResult", "Download All Model Result",style="color: #212121; background-color: #f8bbd0; border-color: #212121")),
              column(3, downloadButton("olsmDownloadFullDecomp", "Download Full Decomposition",style="color: #212121; background-color: #f8bbd0; border-color: #212121")),
              column(2, actionButton("olsm_backmodelManager","Back",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4"))
            )
        )
      )
    }else if(input$olsmMixedModelChoice == "Yes"){
      fluidRow(
        box(title = "Model Result Details", status = "success", width = "12", solidHeader = T, collapsible = F,
            fluidRow(
              column(2, downloadButton("olsmDownLoadModelResult", "Download Model",style="color: #212121; background-color: #ffc8b3; border-color: #212121; float:left")),
              column(3, downloadButton("olsmDownloadFullDecomp", "Download Full Decomposition",style="color: #212121; background-color: #f8bbd0; border-color: #212121")),
              column(2, actionButton("olsm_backmodelManager","Back",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4"))
            )
        )
      )
    }
  })
  
  observeEvent(input$olsm_backmodelManager,{
    updateTabItems(session, "sidebartabs", "olsm_modelManager")
  })
  
  # function to download master dataset.
  output$olsmDownLoadConsolidateResult <- downloadHandler(
    filename = function() {
      paste('AllModelResultData', '.csv', sep='')
    },
    content = function(file) {
      withProgress(message = 'Downloading file will take time.', detail = 'Merging of all Models deatils are in progress....', value = 0.85, {
        olsmModelDataDf <- olsmExtractAllModelData(olsmAllModelList, olsm_parametersDF,input$olsmHasIntercept)
      })
      write.csv(olsmModelDataDf, file, row.names = FALSE)
    }
  )
  
  # function to download model result
  output$olsmDownLoadModelResult <- downloadHandler(
    filename = function() {
      
      if(stackedModel == FALSE){
        if(length(input$olsm_resultTable_rows_selected)==1){
          i  <- input$olsm_resultTable_rows_selected
        }else {
          i  <- 1
        }
        paste(paste0(olsmModelResult[i,1],"_results"), "zip", sep = ".")
      }else if(stackedModel == TRUE){
        if(input$olsmMixedModelChoice == "No"){
          if(length(input$olsm_resultTable_rows_selected)==1){
            i  <- input$olsm_resultTable_rows_selected
          }else {
            i  <- 1
          }
          paste(paste0(olsmModelResult[i,1],"_results"), "zip", sep = ".")
        }else if(input$olsmMixedModelChoice == "Yes"){
          paste("MixedModelResult.zip")
        }
      }
    },
    content = function(file) {
      withProgress(message = 'Downloading file will take time.', detail = 'File building is in progress....', value = 0.85, {
        olsmModelResultTemp   <- NULL
        regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        
        modelFeatureList <- NULL
        modelFeatureList <- list(olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos,input$olsmMixedModelChoice, stackedModel,input$olsmHasIntercept)
        names(modelFeatureList) <- c("modellingPeriod", "selectedGeos","mixedModelChioce", "stackedModel","hasIntercept") 
        
        if(input$olsmMixedModelChoice == "No"){
          if(length(input$olsm_resultTable_rows_selected)==1){
            model <- olsmAllModelList[[input$olsm_resultTable_rows_selected]]
            index  <- input$olsm_resultTable_rows_selected
            modelResult <- olsmModelResult[index,]
            if(grepl("Dummy",olsmModelResult[index,"Model_No"])){
              dummyScope <- olsmDummyModelDateScope[[olsmModelResult[index,"Model_No"]]]
              regDf <- data.frame(subset(regDf, dmy(regDf$Period) >= dummyScope$dummyStartDate & dmy(regDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
            }
          }else {
            index <- 1
            model <- olsmAllModelList[[1]]
            modelResult <- olsmModelResult[1,]
          }
          obsCount <- nrow(olsmFinalTransRegDf)
          olsmModelData <- olsmFinalNormRegDf
          if(!any(names(olsmModelData) %in% "Period")){
            olsmModelData <- cbind(olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
            names(olsmModelData) <- c("Period", names(olsmFinalNormRegDf))
          }
          olsmModelData$Period <- lubridate::dmy(olsmModelData$Period)
          
          # transData is used in elasticity calculation
          transData <- olsmFinalTransRegDf
          transData$Period <- lubridate::dmy(transData$Period)
          if(grepl("Dummy", olsmModelResult[index,"Model_No"])){
            dummyModelData  <- subset(olsmModelData, Period >= olsmDummyModelDateScope[[olsmModelResult[index,"Model_No"]]]$dummyStartDate & Period <= olsmDummyModelDateScope[[olsmModelResult[index,"Model_No"]]]$dummyEndDate)
            olsmModelData <- cbind(dummyModelData, model$model[,grep("Dummy",names(model$model))])
            names(olsmModelData) <- c(names(dummyModelData), names(model$model)[grep("Dummy",names(model$model))])
            
            # transData is used in elasticity calculation
            transData <- subset(transData, Period >= olsmDummyModelDateScope[[olsmModelResult[index,"Model_No"]]]$dummyStartDate & Period <= olsmDummyModelDateScope[[olsmModelResult[index,"Model_No"]]]$dummyEndDate)
          }
          
          olsmFullDecomp <- olsmExtractFullDecomp(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp)
          depVar <- as.character(olsm_parametersDF$VariableName[which(olsm_parametersDF$Type == "DepVar")])
          modelContr <- olsmGetContribution(olsmFullDecomp, depVar,unrolled = NULL)
          
          olsmResult <- olsmGetModelParameter(model, olsm_parametersDF,input$olsmHasIntercept, modelContr, transData)
          output <- as.data.frame(olsmExtractModelDetail(model, modelResult,olsm_parametersDF, obsCount,olsmResult), quote=F)
          actPredData <- olsm_getActualVsPredictedDf(model, olsm_parametersDF, regDf)
        }else if(input$olsmMixedModelChoice == "Yes"){
          model <- olsmAllModelList[[1]]
          modelParam <- list()
          modelParam <- list(olsmModelParameter$olsm_SelectedGeos,olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"], nrow(olsm_RegDataTemp))
          names(modelParam) <- c("SelectedGeos","Depvar","TotalDataCount") 
          actPredData <- data.frame(Geography = model$groups,Period = regDf$Period,Actual = model$data[,modelParam$Depvar],model$fitted,model$residuals)
          names(actPredData) <- c("Geography","Period","Actual","Fixed_Fitted","Random_Fitted","Fixed_Residuals","Random_Residuals")
          
          output <- as.data.frame(olsmExtractMixedModel(model,modelParam,olsmFinalNormRegDf,modelFeatureList,olsm_parametersDF,olsm_RegDataTemp), quote=F)
        }
        
        colnames(output) <- "output"
        output <- cSplit(output,"output",sep = ",",type.convert = F)
        olsmModelResultTemp[["ModelParameter"]] <- output
        
        if(stackedModel == TRUE){
          actPredDenormList <- olsmDenormActvsPred(modelFeatureList, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          olsmModelResultTemp[["actPredDataUnRolled"]] <- actPredDenormList[["actPredDataUnRolled"]]
          olsmModelResultTemp[["actPredDataRolled"]] <- actPredDenormList[["actPredDataRolled"]]
        }else if(stackedModel == FALSE){
          olsmModelResultTemp[["actPredData"]] <- olsmDenormActvsPred(modelFeatureList, actPredData, olsm_parametersDF, olsm_RegDataTemp)
        }
        
        # Model data
        if(input$olsmMixedModelChoice == "No"){
          if(!any(names(olsmFinalRegDf) %in% "Period")){
            olsmFinalRegDf <- cbind(Period = olsmFinalTransRegDf$Period, olsmFinalRegDf)
          }
          olsmModelResultTemp[["ModelData"]] <- olsmExtractModelData(model, olsm_parametersDF, olsmFinalTransRegDf, olsmFinalRegDf,regDf,olsmDummyModelDateScope,modelResult)
        }else if(input$olsmMixedModelChoice == "Yes"){
          olsmModelResultTemp[["ModelData"]] <- data.frame(Geography = model$data$Geography,Period = regDf$Period,model$data[-1])
        }
        
        # Writing all files as csv and zipped while downloading. Using Temp folder for writing csv files.
        wd <- getwd()
        setwd(tempdir())
        files <- NULL
        for(name in names(olsmModelResultTemp)){
          fileName <- paste0(name, ".csv")
          write.csv(x = olsmModelResultTemp[[name]], file = paste0(name,".csv"), row.names = FALSE,na = "",col.names = FALSE)
          files <- c(fileName,files)
        }
        zip(file,files)
        setwd(wd)
      })
    }
  )
  
  # function to download Full Decomp Data.
  output$olsmDownloadFullDecomp <- downloadHandler(
    filename = function() {
      if(stackedModel == FALSE){
        if(length(input$olsm_resultTable_rows_selected)==1){
          i  <- input$olsm_resultTable_rows_selected
        }else {
          i  <- 1
        }
        paste(paste0(olsmModelResult[i,1],"_FullDecomposition"), "csv", sep = ".")
      }else if(stackedModel == TRUE){
        if(input$olsmMixedModelChoice == "No"){
          if(length(input$olsm_resultTable_rows_selected)==1){
            i  <- input$olsm_resultTable_rows_selected
          }else {
            i  <- 1
          }
          paste(paste0(olsmModelResult[i,1],"_FullDecomposition"), "zip", sep = ".")
        }else if(input$olsmMixedModelChoice == "Yes"){
          paste("MixedModelResult_FullDecomposition.zip")
        }
      }
    },
    content = function(file) {
      i  <- input$olsm_resultTable_rows_selected
      if(length(i)==0){
        i <- 1
        model <- olsmAllModelList[[1]]
      }else{
        model <- olsmAllModelList[[i]]
      }
      
      olsmModelData <- olsmFinalNormRegDf
      if(!any(names(olsmModelData) %in% "Period")){
        olsmModelData <- cbind(olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        names(olsmModelData) <- c("Period", names(olsmFinalNormRegDf))
      }
      olsmModelData$Period <- lubridate::dmy(olsmModelData$Period)
      
      if(length(olsmDummyModelDateScope)!=0){
        if(grepl("Dummy", olsmModelResult[i,"Model_No"])){
          dummyModelData  <- subset(olsmModelData, Period >= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyStartDate & Period <= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyEndDate)
          olsmModelData <- cbind(dummyModelData, model$model[,grep("Dummy",names(model$model))])
          names(olsmModelData) <- c(names(dummyModelData), names(model$model)[grep("Dummy",names(model$model))])
        }
      }
      
      modelFeatureList <- NULL
      modelFeatureList <- list(olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos,input$olsmMixedModelChoice, stackedModel,input$olsmHasIntercept)
      names(modelFeatureList) <- c("modellingPeriod", "selectedGeos","mixedModelChioce", "stackedModel","hasIntercept") 
      olsmFullDecomp <- olsmExtractFullDecomp(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp)
      
      # changing working directory to temp folder to keep all .csv file before writing.
      if(stackedModel == TRUE){
        wd <- getwd()
        setwd(tempdir())
        files <- NULL
        for(name in names(olsmFullDecomp)){
          fileName <- paste0(name, ".csv")
          write.csv(x = olsmFullDecomp[[name]], file = paste0(name,".csv"), row.names = FALSE)
          files <- c(fileName,files)
        }
        zip(file,files)
        setwd(wd)
      }else if(stackedModel == FALSE){
        write.csv(olsmFullDecomp[["Fulldecomposition_BaseModel"]], file, row.names = FALSE)
        
      }
      
    }
  )
  
  # UI render code to display olsm model result table. It is UI function, don't consider as Modeller function.
  olsm_displayUIResultTable <- function(stacked, splitVar){
    
    if(input$olsmMixedModelChoice == "No"){
      
      output$olsm_resultTable <- renderDataTable({
        
        outsideVar <- olsm_parametersDF$VariableName[grep("Outside",olsm_parametersDF$Type)]
        outsideTstat <- as.data.frame(unlist(lapply(olsmAllModelList, function(x, outsideVar) olsmExtractOutsideTstat(x, outsideVar), outsideVar = outsideVar)))
        colnames(outsideTstat) <- c("Outside_Var_Tstat")
        olsmModelResult <<- cbind(olsmModelResult, "Outside_Var_Tstat" = outsideTstat)
        
        datatable({
          olsmModelResult
        }, rownames = FALSE, class = 'cell-border stripe',extensions = 'Buttons',
        options = list(dom = 'Bfrtip',autoWidth = FALSE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       paging = TRUE,scrollX = TRUE,scrollY = TRUE,searching = FALSE,bInfo = FALSE
        ), selection = list(mode = "single"), height = "2000px")%>%
          formatRound(columns=c(3,4,5), digits=2)%>%
          formatRound(columns=c(6), digits=5)
      })
      
      output$olsm_ModelResult <- renderUI({
        i  <- input$olsm_resultTable_rows_selected
        if(length(i) <= 1){
          if(stacked == FALSE){
            tagList(
              fluidRow(
                box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsmModelParameters"),
                    fluidRow(
                      column(12, actionButton("olsmAddDummyButton","Add Dummy", icon = icon("list-alt"),style="color: #ffffff; background-color: #ec407a ; border-color: #212121; float:right"))
                    )
                )
              ),
              fluidRow(
                box(title = "Actual V/S Predicted Plot",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = F,
                    wellPanel(
                      plotlyOutput("olsmModelAvmChart", width = "100%", height = "100%")
                    )
                )
              ),
              fluidRow(
                box(title = "Actual V/S Predicted Data",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = T,
                    wellPanel(
                      dataTableOutput("olsmModelAVMData")
                    )
                )
              )
            )
          }else if(stacked == TRUE){
            tagList(
              fluidRow(
                box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsmModelParameters"),
                    fluidRow(
                      column(12, actionButton("olsmAddDummyButton","Add Dummy", icon = icon("list-alt"),style="color: #ffffff; background-color: #ec407a ; border-color: #212121; float:right"))
                    )
                )
              ),
              fluidRow(
                box(title = "Actual V/S Predicted Plot",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = F,
                    wellPanel(
                      selectInput("olsmSplitColumnLevel",label = "Select Geography: ",choices = c(olsmModelParameter$olsm_SelectedGeos,"RolledUP") ,multiple = F,selectize = F,selected = "RolledUP")
                    ),  
                    wellPanel(
                      plotlyOutput("olsmModelAvmChart", width = "100%", height = "100%")
                    )
                )
              ),
              fluidRow(
                box(title = "Actual V/S Predicted Data",status = "success",solidHeader = T,width = 12,collapsible = T,collapsed = T,
                    wellPanel(
                      dataTableOutput("olsmModelAVMData")
                    )
                )
              )
            )
          }
        }
      })
      
      output$olsmModelParameters <- renderDataTable({
        
        i  <- input$olsm_resultTable_rows_selected
        if(length(i)==0){
          model <- olsmAllModelList[[1]]
        }else{
          model <- olsmAllModelList[[i]]
        }
        
        olsmModelData <- olsmFinalNormRegDf
        if(!any(names(olsmFinalNormRegDf) %in% "Period")){
          olsmModelData <- cbind(olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
          names(olsmModelData) <- c("Period", names(olsmFinalNormRegDf))
        }
        olsmModelData$Period <- lubridate::dmy(olsmModelData$Period)
        
        # transData is used in elasticity calculation
        transData <- olsmFinalTransRegDf
        transData$Period <- lubridate::dmy(transData$Period)
        if(length(i)!=0){
          if(grepl("Dummy", olsmModelResult[i,"Model_No"])){
            dummyModelData  <- subset(olsmModelData, Period >= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyStartDate & Period <= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyEndDate)
            olsmModelData <- cbind(dummyModelData, model$model[,grep("Dummy",names(model$model))])
            names(olsmModelData) <- c(names(dummyModelData), names(model$model)[grep("Dummy",names(model$model))])
            # transData is used in elasticity calculation
            transData <- subset(transData, Period >= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyStartDate & Period <= olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]$dummyEndDate)
          }
        }
        
        modelFeatureList <- NULL
        modelFeatureList <- list(olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos,input$olsmMixedModelChoice, stackedModel,input$olsmHasIntercept)
        names(modelFeatureList) <- c("modellingPeriod", "selectedGeos","mixedModelChioce", "stackedModel","hasIntercept") 
        olsmFullDecomp <- olsmExtractFullDecomp(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp)
        depVar <- as.character(olsm_parametersDF$VariableName[which(olsm_parametersDF$Type == "DepVar")])
        modelContr <- olsmGetContribution(olsmFullDecomp, depVar, unrolled = NULL)
        
        olsmResult <- olsmGetModelParameter(model, olsm_parametersDF,input$olsmHasIntercept, modelContr, transData)
        
        datatable({
          olsmResult
        },rownames = FALSE, extensions = 'Buttons',options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = TRUE,
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ), selection = 'none') %>%
          formatRound(columns=c(2,3), digits=5)%>%
          formatRound(columns=c(4:9), digits=2)
      })
      
      output$olsmModelAvmChart <- renderPlotly({
        i  <- input$olsm_resultTable_rows_selected
        if(length(i)==0){
          model <- olsmAllModelList[[1]]
          regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        }else{
          model <- olsmAllModelList[[i]]
          if(grepl("Dummy",olsmModelResult[i,"Model_No"])){
            dummyScope <- olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]
            regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
            regDf <- data.frame(subset(regDf, dmy(regDf$Period) >= dummyScope$dummyStartDate & dmy(regDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
          }else{
            regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
          }
        }
        
        actPredData <- olsm_getActualVsPredictedDf(model, olsm_parametersDF, regDf)
        modelParam <- list()
        modelParam <- list(stackedModel, olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos, input$olsmMixedModelChoice)
        names(modelParam) <- c("stackedModel","modellingPeriod", "selectedGeos","mixedModelChioce") 
        
        if(stackedModel == TRUE){
          actPredDenormList <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          actPredDenormList$actPredDataUnRolled$Period <- lubridate::dmy(actPredDenormList$actPredDataUnRolled$Period)
          actPredData <- data.frame(rbind(actPredDenormList$actPredDataUnRolled, cbind(Geography = "RolledUP",actPredDenormList$actPredDataRolled)),row.names = NULL)
          actPredData <- actPredData[as.numeric(rownames(subset(actPredData, actPredData[,"Geography"]==input$olsmSplitColumnLevel))),]
        }else if(stackedModel == FALSE){
          actPredData <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          actPredData <- actPredData$actPredData
          actPredData$Period <- lubridate::dmy(actPredData$Period)
        }
        
        date <- actPredData$Period
        p <- plot_ly()
        y_data <- input$colNames
        x_data  <- input$xAxis
        predData <- actPredData$Predicted
        actData <- actPredData$Actual
        residualData <- actPredData$Residual
        
        p <-add_trace(p,x = date,y= predData ,type = "scatter",mode = "markers+lines" ,name = "Fitted")
        p <-add_trace(p,x = date, y = actData ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(model$model[1])))
        p <-add_trace(p,x = date, y = residualData ,type = "bar", mode = "hoverinfo" ,name = "Residual")
        for (name in y_data) {
          p <- add_trace( p,x = date, y = model$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
        }
        p %>% layout(legend = list(orientation = 'h'))
      })
      
      output$olsmModelAVMData <- renderDataTable({
        i  <- input$olsm_resultTable_rows_selected
        if(length(i)==0){
          model <- olsmAllModelList[[1]]
          regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        }else{
          model <- olsmAllModelList[[i]]
          if(grepl("Dummy",olsmModelResult[i,"Model_No"])){
            dummyScope <- olsmDummyModelDateScope[[olsmModelResult[i,"Model_No"]]]
            regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
            regDf <- data.frame(subset(regDf, dmy(regDf$Period) >= dummyScope$dummyStartDate & dmy(regDf$Period) <= dummyScope$dummyEndDate),row.names = NULL)
          }else{
            regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
          }
        }
        
        actPredData <- olsm_getActualVsPredictedDf(model, olsm_parametersDF, regDf)
        modelParam <- list()
        modelParam <- list(stackedModel, olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos, input$olsmMixedModelChoice)
        names(modelParam) <- c("stackedModel","modellingPeriod", "selectedGeos","mixedModelChioce") 
        
        if(stackedModel == TRUE){
          actPredDenormList <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          actPredDenormList$actPredDataUnRolled$Period <- lubridate::dmy(actPredDenormList$actPredDataUnRolled$Period)
          actPredData <- data.frame(rbind(actPredDenormList$actPredDataUnRolled, cbind(Geography = "RolledUP",actPredDenormList$actPredDataRolled)),row.names = NULL)
          actPredData <- actPredData[as.numeric(rownames(subset(actPredData, actPredData[,"Geography"]==input$olsmSplitColumnLevel))),]
          actPredData[,c(3,4,5)] <- round(actPredData[,c(3,4,5)],digits = 5)
        }else if(stackedModel == FALSE){
          actPredData <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          actPredData <- actPredData$actPredData
          actPredData$Period <- lubridate::dmy(actPredData$Period)
          actPredData[,c(2,3,4)] <- round(actPredData[,c(2,3,4)],digits = 5)
        }
        
        datatable({
          actPredData
        },rownames = FALSE, extensions = 'Buttons',options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = TRUE,
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ), selection = 'none')
      })
      
    }else if(input$olsmMixedModelChoice == "Yes"){
      
      output$olsm_ModelResult <- renderUI({
        
        tagList(
          fluidRow(
            box(title = "Rolled-up Estimate",status = "success",solidHeader = T,width = 12,
                dataTableOutput("olsmFixedModelParameters")
                
            )
          ),
          fluidRow(
            box(title = "Estimate by Geography (Unrolled)",status = "success",solidHeader = T,width = 12,
                dataTableOutput("olsmRandomModelParameters")
            )
          )
        )
      })
      
      output$olsmFixedModelParameters <- renderDataTable({
        RolledupResult <- NULL
        model <- olsmAllModelList[[1]]
        RolledupResult <- as.data.frame(summary(model)$tTable)[,c("Value","t-value","p-value")]
        RolledupResult <- data.frame(term = rownames(RolledupResult), RolledupResult, row.names = NULL,stringsAsFactors = F)
        names(RolledupResult) <- c("term","Rolledup_Estimate","Rolledup_t_value","Rolledup_p_value")
        
        if(any(grepl("Combined",RolledupResult$term))){
          RolledupResult <- data.frame(olsmSplitCombinedEstimateData(RolledupResult, olsm_parametersDF),row.names = NULL)
        }
        if(any(grepl("(Intercept)",RolledupResult$term))){
          RolledupResult$term[grep("(Intercept)",RolledupResult$term)] <- "Intercept"
        }
        
        # calculating Contribution
        olsmModelData <- olsmFinalNormRegDf
        olsmModelData$Period <- lubridate::dmy(olsmModelData$Period)
        modelFeatureList <- NULL
        modelFeatureList <- list(olsmModelParameter$olsm_SelectedPeriod,olsmModelParameter$olsm_SelectedGeos,input$olsmMixedModelChoice, stackedModel,input$olsmHasIntercept)
        names(modelFeatureList) <- c("modellingPeriod", "selectedGeos","mixedModelChioce", "stackedModel","hasIntercept")
        olsmFullDecomp <- olsmExtractFullDecomp(model, olsm_parametersDF, olsmModelData,modelFeatureList, olsm_RegDataTemp)
        depVar <- olsm_parametersDF$VariableName[olsm_parametersDF$Type == "DepVar"]
        RolledupResult <- merge(RolledupResult,olsmGetContribution(olsmFullDecomp, depVar, unrolled = NULL),by="term",sort = F)
        
        datatable({
          RolledupResult
        },rownames = FALSE, extensions = 'Buttons',options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = TRUE,
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ), selection = 'none') %>%
          formatRound(columns=c(2,3,4,5), digits=6)
      })
      
      output$olsmRandomModelParameters <- renderDataTable({
        RandomModel <- NULL
        RandomresultTable <- data.frame(term = rownames(t(coef(olsmAllModelList[[1]]))),data.frame(t(coef(olsmAllModelList[[1]])),row.names = NULL))
        if(any(grepl("Combined",RandomresultTable$term))){
          RandomresultTable <- data.frame(olsmSplitCombinedEstimateData(RandomresultTable, olsm_parametersDF),row.names = NULL)
        }
        datatable({
          RandomresultTable
        },rownames = FALSE, extensions = 'Buttons',options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = TRUE,
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ), selection = 'none') %>%
          formatRound(columns=c(-1), digits=6)
      })
      
    }
    
  }
  
  
  ################################### OLSM Dummy Model ###########################################
  #--------------------------------------------------------------------------
  # Function to add dummy variable to candidate model.
  #--------------------------------------------------------------------------
  
  # to open the pop up for dummy variable addition
  observeEvent(input$olsmAddDummyButton, {
    updateTabsetPanel(session, "olsmResultTabset", selected = "panel2")
  })
  
  output$olsmDummyModelScopeUI <- renderUI({
    start_date <- min(lubridate::dmy(olsmModelParameter$olsm_SelectedPeriod))
    end_date <- max(lubridate::dmy(olsmModelParameter$olsm_SelectedPeriod))
    tagList(
      box(title = "Select Dummy Model scope",status = "warning",width = "12",solidHeader = T,
          fluidRow(
            column(5,dateInput('olsmDummyStartDate', label = "Modelling start Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = start_date,min = start_date,max = end_date)),
            column(5,dateInput('olsmDummyEndDate', label = "Modelling end Date (dd/mm/yyyy)", format = "dd/mm/yyyy",value = end_date,min = start_date,max = end_date)),
            column(2,actionButton("olsmDummyModelScope", "Continue",style="color: #212121; background-color: #fbc998; border-color: #2e6da4"))
          )
      )
    )
  })
  
  observeEvent(input$olsmDummyModelScope,{
    output$olsmDummyVarTableUI <- renderUI({
      tagList(
        box(title = "Provide Dummy Model Variable value",status = "warning",width = "12",solidHeader = T,
            wellPanel(
              rHandsontableOutput("olsmDummyVarTable"),
              br(),
              fluidRow(
                column(12, actionButton("olmsSubmitDummyVar","Submit", icon = icon("table"),style="color: #212121; background-color: #ffcdd2; border-color: #212121; float: right"))
              )
            )
        ),
        uiOutput("olsmDummyModelDetailPop")
      )
    })
  })
  
  olsmModelScopeDummyTable <- NULL
  
  output$olsmDummyVarTable <- renderRHandsontable({
    olsmModelScopeDummyTable <- olsmDummyData()
    if (!is.null(olsmModelScopeDummyTable)){
      rhandsontable(olsmModelScopeDummyTable, 
                    height = 400,useTypes = TRUE,stretchH = "all",selectCallback = T,
                    fillHandle = list(direction='vertical', autoInsertRow=FALSE), 
                    maxRows = nrow(olsmModelScopeDummyTable)) %>%
        hot_col("Period", readOnly = TRUE)%>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  olsmDummySDateTmp <- NULL
  olsmDummyEDateTmp <- NULL
  
  olsmDummyData <- reactive({
    olsm_RegDataTemp <- olsm_RegDataTemp[olsm_RegDataTemp$Period %in% olsmModelParameter$olsm_SelectedPeriod,]
    olsm_RegDataTemp$Period <- lubridate::dmy(olsm_RegDataTemp$Period)
    if(!is.null(input$olsmDummyVarTable)){
      dummyDF <- hot_to_r(input$olsmDummyVarTable)
      if(olsmDummySDateTmp != input$olsmDummyStartDate | olsmDummyEDateTmp != input$olsmDummyEndDate){
        dummyModelScopeDf <- subset(olsm_RegDataTemp, Period >= input$olsmDummyStartDate & Period <= input$olsmDummyEndDate)
        if(stackedModel == FALSE){
          dummyDF <- data.frame(
            Period = dummyModelScopeDf$Period,
            Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            row.names = c(1:nrow(dummyModelScopeDf)),
            stringsAsFactors = FALSE )
        }else if(stackedModel == TRUE){
          dummyModelScopeDf <- dummyModelScopeDf[dummyModelScopeDf$Geography %in% input$olsm_SelectedGeos,]
          dummyDF <- data.frame(
            Geography = dummyModelScopeDf$Geography,
            Period = dummyModelScopeDf$Period,
            Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            row.names = c(1:nrow(dummyModelScopeDf)),
            stringsAsFactors = FALSE )
        }
        olsmDummySDateTmp <<- input$olsmDummyStartDate
        olsmDummyEDateTmp <<- input$olsmDummyEndDate
      }
    }else {
      if (is.null(olsm_values[["dummyDF"]])){
        dummyModelScopeDf <- subset(olsm_RegDataTemp, Period >= input$olsmDummyStartDate & Period <= input$olsmDummyEndDate)
        if(stackedModel == FALSE){
          dummyDF <- data.frame(
            Period = dummyModelScopeDf$Period,
            Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            row.names = c(1:nrow(dummyModelScopeDf)),
            stringsAsFactors = FALSE )
        }else if(stackedModel == TRUE){
          dummyModelScopeDf <- dummyModelScopeDf[dummyModelScopeDf$Geography %in% olsmModelParameter$olsm_SelectedGeos,]
          dummyDF <- data.frame(
            Geography = dummyModelScopeDf$Geography,
            Period = dummyModelScopeDf$Period,
            Dummy_Var1=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var2=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            Dummy_Var3=rep(as.numeric(0), times = nrow(dummyModelScopeDf)),
            row.names = c(1:nrow(dummyModelScopeDf)),
            stringsAsFactors = FALSE )
        }
        olsmDummySDateTmp <<- input$olsmDummyStartDate
        olsmDummyEDateTmp <<- input$olsmDummyEndDate
      }
      else
        dummyDF = olsm_values[["dummyDF"]]
    }
    olsm_values[["dummyDF"]] = dummyDF
    dummyDF
  })
  
  olsmDummyModel <- NULL
  olsm.model.index <- NULL
  
  # to close the pop up for dummy variable addition
  observeEvent(input$olmsSubmitDummyVar, {
    olsmModelScopeDummyTable <<- isolate(olsm_values[["dummyDF"]])
    
    if(is.null(input$olsm_resultTable_rows_selected)){
      olsm.model.index <<- 1
    }else{
      olsm.model.index <<- as.numeric(rownames(olsmModelResult[input$olsm_resultTable_rows_selected,]))
    }
    
    if(all(as.vector(apply(olsmModelScopeDummyTable[,!names(olsmModelScopeDummyTable) %in% c("Geography","Period")],2,sum)) == 0)){
      meCustomAlert(message = "Empty Dummy Table, Please provide values to atleast one dummy variable.",alertType = "warning")
      toggleModal(session, "olsmDummyModel", toggle = "close")
    }else {
      
      if(stackedModel == FALSE){
        finalDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalRegDf)
      }else if(stackedModel == TRUE){
        finalDf <- olsmFinalRegDf
      }
      
      dummyModelProp <- as.list(c(olsm.model.index,input$olsmWLSChoice, input$olsmHasIntercept,stackedModel))
      names(dummyModelProp) <- c("olsm.model.index", "WLSChoice", "hasIntercept","stackedModel")
      
      olsmDummyModel <<- olsmGetDummyModelResult(olsmAllModelList, olsmModelResult,olsmModelScopeDummyTable,finalDf, olsm_parametersDF,dummyModelProp)
      
      olsmDummyModel <<- olsmDummyModel[[1]]
      olsmDummyModelList <- as.list(NULL)
      olsmDummyModelList[[1]] <- olsmDummyModel
      olsmDummyModelResult <- olsmExtractModelParameter(olsmDummyModelList, olsm_parametersDF)
      olsmDummyModelResult$Model_No <- "Dummy Model"
      
      output$olsmDummyModelResult = renderDataTable({
        datatable({
          olsmDummyModelResult
        },rownames = FALSE, extensions = 'Buttons',
        options = list(
          scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),paging = FALSE,searching = FALSE,bInfo = FALSE
        ), selection = 'none')%>%
          formatRound(columns=c(4,5), digits=5) 
      })
      
      output$olsDummyModelParameters = renderDataTable({
        
        dummyDFTable <- as.data.frame(olsmModelScopeDummyTable[,which(names(olsmModelScopeDummyTable) %in% names(which(apply(olsmModelScopeDummyTable[,!names(olsmModelScopeDummyTable) %in% c("Geography","Period")],2,sum)!=0)))])
        names(dummyDFTable) <- names(which(apply(olsmModelScopeDummyTable[,!names(olsmModelScopeDummyTable) %in% c("Geography","Period")],2,sum)!=0))
        olsmDummyData <- cbind(olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        names(olsmDummyData) <- c("Period", names(olsmFinalNormRegDf))
        olsmDummyData$Period <- lubridate::dmy(olsmDummyData$Period)
        dummyModelData  <- subset(olsmDummyData, Period >= min(olsmModelScopeDummyTable$Period) & Period <= max(olsmModelScopeDummyTable$Period))
        
        olsmDummyData <- cbind(dummyModelData, dummyDFTable)
        dummyPeriod <- olsm_RegDataTemp$Period[dmy(olsm_RegDataTemp$Period) %in% olsmModelScopeDummyTable$Period]
        modelFeatureList <- NULL
        modelFeatureList <- list(dummyPeriod,olsmModelParameter$olsm_SelectedGeos,input$olsmMixedModelChoice, stackedModel,input$olsmHasIntercept)
        names(modelFeatureList) <- c("modellingPeriod", "selectedGeos","mixedModelChioce", "stackedModel","hasIntercept") 
        olsmFullDecomp <- olsmExtractFullDecomp(olsmDummyModel, olsm_parametersDF, olsmDummyData,modelFeatureList, olsm_RegDataTemp)
        depVar <- as.character(olsm_parametersDF$VariableName[which(olsm_parametersDF$Type == "DepVar")]) 
        modelContr <- olsmGetContribution(olsmFullDecomp, depVar, unrolled = NULL)
        
        # transData is used in elasticity calculation
        transData <- olsmFinalTransRegDf
        transData$Period <- lubridate::dmy(transData$Period)
        transData <- subset(transData, Period >= min(olsmModelScopeDummyTable$Period) & Period <= max(olsmModelScopeDummyTable$Period))
        dummyModelResultDetail <- olsmGetModelParameter(olsmDummyModel, olsm_parametersDF,input$olsmHasIntercept, modelContr, transData)
        
        datatable({
          dummyModelResultDetail
        },rownames = FALSE, extensions = 'Buttons',options = list(
          scrollX = TRUE,buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),paging = FALSE,searching = FALSE,bInfo = FALSE
        ), selection = 'none')%>%
          formatRound(columns=c(2,3,4,5,6,7,8,9), digits=5) 
      })
      
      output$olsmDummyModelAvmChart <- renderPlotly({
        dummyModel <- olsmDummyModel
        regDf <- cbind(Period = olsmFinalTransRegDf$Period,olsmFinalNormRegDf)
        regDf[,"Period"] <- lubridate::dmy(regDf$Period)
        regDf <- data.frame(subset(regDf,Period >= input$olsmDummyStartDate & Period <= input$olsmDummyEndDate),row.names = NULL)
        actPredData <- olsm_getActualVsPredictedDf(dummyModel, olsm_parametersDF, regDf)
        
        dummyPeriod <- lubridate::dmy(olsmModelParameter$olsm_SelectedPeriod)
        dummyPeriod <- olsmModelParameter$olsm_SelectedPeriod[dummyPeriod >= input$olsmDummyStartDate & dummyPeriod <= input$olsmDummyEndDate]
        
        modelParam <- list()
        modelParam <- list(stackedModel, dummyPeriod,olsmModelParameter$olsm_SelectedGeos, input$olsmMixedModelChoice)
        names(modelParam) <- c("stackedModel","modellingPeriod", "selectedGeos","MixedModelChoice") 
        
        if(stackedModel == TRUE){
          actPredDenormList <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          if(class(actPredDenormList$actPredDataUnRolled$Period) != "Date"){
            actPredDenormList$actPredDataUnRolled$Period <- lubridate::dmy(actPredDenormList$actPredDataUnRolled$Period)
          }
          actPredData <- data.frame(rbind(actPredDenormList$actPredDataUnRolled, cbind(Geography = "RolledUP",actPredDenormList$actPredDataRolled)),row.names = NULL)
          actPredData <- actPredData[as.numeric(rownames(subset(actPredData, actPredData[,"Geography"]==input$olsmDummyGeoSelected))),]
        }else if(stackedModel == FALSE){
          actPredData <- olsmDenormActvsPred(modelParam, actPredData, olsm_parametersDF, olsm_RegDataTemp)
          actPredData <- actPredData$actPredData
          if(class(actPredData$Period) != "Date"){
            actPredData$Period <- lubridate::dmy(actPredData$Period)
          }
        }
        
        date <- actPredData$Period
        p <- plot_ly()
        y_data <- input$colNames
        x_data  <- input$xAxis
        predData <- actPredData$Predicted
        actData <- actPredData$Actual
        residualData <- actPredData$Residual
        
        p <-add_trace(p,x = date,y= predData ,type = "scatter",mode = "markers+lines" ,name = "Fitted")
        p <-add_trace(p,x = date, y = actData ,type = "scatter", mode = "markers+lines" ,name = paste("Actual_", names(dummyModel$model[1])))
        p <-add_trace(p,x = date, y = residualData ,type = "bar", mode = "hoverinfo" ,name = "Residual")
        for (name in y_data) {
          p <- add_trace( p,x = date, y = model$model[, name] , type = "scatter", mode = "markers+lines" ,text = paste(name))
        }
        p %>% layout(legend = list(orientation = 'h'))
      })
    }
  })
  
  output$olsmDummyModelDetailPop <- renderUI({
    if(stackedModel== TRUE){
      bsModal(id = "olsmDummyModel",title = "Dummy Model Result", trigger = "olmsSubmitDummyVar",size = "large",
              tagList(
                box(title = "Model Result",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsmDummyModelResult")
                ),
                box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsDummyModelParameters")
                ),
                box(title = "Actual V/S Predicted",status = "success",solidHeader = T,width = 12,
                    wellPanel(
                      selectInput("olsmDummyGeoSelected",label = "Select Geography: ",choices = c(olsmModelParameter$olsm_SelectedGeos,"RolledUP") ,multiple = F,selectize = F)
                    ),
                    wellPanel(
                      plotlyOutput("olsmDummyModelAvmChart", width = "100%", height = "100%")
                    )
                )
              ),
              fluidRow(
                column(1, actionButton("olsmDummyModelModify","Modify",style="color: #212121; background-color: #ffc8b3; border-color: #212121")),
                column(2, actionButton("olsmDummyModelSubmit","Submit",style="color: #212121; background-color: #ffc8b3; border-color: #212121"))
              )
      )
    }else if(stackedModel== FALSE){
      bsModal(id = "olsmDummyModel",title = "Dummy Model Result", trigger = "olmsSubmitDummyVar",size = "large",
              tagList(
                box(title = "Model Result",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsmDummyModelResult")
                ),
                box(title = "Model Parameters",status = "success",solidHeader = T,width = 12,
                    dataTableOutput("olsDummyModelParameters")
                ),
                box(title = "Actual V/S Predicted",status = "success",solidHeader = T,width = 12,
                    wellPanel(
                      plotlyOutput("olsmDummyModelAvmChart", width = "100%", height = "100%")
                    )
                )
              ),
              fluidRow(
                column(1, actionButton("olsmDummyModelModify","Modify",style="color: #212121; background-color: #ffc8b3; border-color: #212121")),
                column(2, actionButton("olsmDummyModelSubmit","Submit",style="color: #212121; background-color: #ffc8b3; border-color: #212121"))
              )
      )
    }
  })
  
  observeEvent(input$olsmDummyModelModify,{
    toggleModal(session, "olsmDummyModel", toggle = "close")
  })
  
  olsmDummyModelScopeDf <- NULL
  
  # store the dummy model dates in a table to plot the AVM in main result page and include in Model result download.
  olsmDummyModelDateScope <- as.list(NULL)
  
  observeEvent(input$olsmDummyModelSubmit,{
    tmpList <- NULL
    modelNumber <- as.numeric(gsub("_.*","",gsub("Model_","",olsmModelResult[olsm.model.index,"Model_No"])))
    olsmAllModelList[[length(olsmAllModelList)+1]] <<- olsmDummyModel
    tmpList[[1]] <- olsmDummyModel
    tmpResult <- olsmExtractModelParameter(tmpList, olsm_parametersDF)
    
    olsmModelResult <<- olsmModelResult[,1:6]
    olsmModelResult$Model_No <<- as.character(olsmModelResult$Model_No)
    olsmModelResult  <<- rbind(olsmModelResult, tmpResult)
    modelNumber <- as.numeric(strsplit(as.character(olsmModelResult$Model_No[olsm.model.index]), split = "_")[[1]][2])
    olsmDummyModelsList[[modelNumber]] <<- olsmDummyModelsList[[modelNumber]]+1
    olsmModelResult[nrow(olsmModelResult),"Model_No"] <<- paste0("Model_",modelNumber,"_Dummy_",as.numeric(olsmDummyModelsList[[modelNumber]]))
    
    
    olsmDummyModelDateScope[[paste0("Model_",modelNumber,"_Dummy_",as.numeric(olsmDummyModelsList[[modelNumber]]))]] <<- data.frame(dummyStartDate = input$olsmDummyStartDate, dummyEndDate = input$olsmDummyEndDate)
    
    # calling function to display UI of olsm Model Result Table.
    if(stackedModel==FALSE){
      olsm_displayUIResultTable(stacked = FALSE,splitVar = olsm_splitByGeoSubset)
    }else if(stackedModel==TRUE){
      olsm_displayUIResultTable(stacked = TRUE,splitVar = olsm_splitByGeoSubset)
    }
    toggleModal(session, "olsmDummyModel", toggle = "close")
    updateTabsetPanel(session, "olsmResultTabset",selected = "panel1")
  })
  
})  