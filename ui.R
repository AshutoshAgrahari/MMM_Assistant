# This is the user-interface of ModellerEngine application version 1.4.

dashboardPage(
  title = "ModellerEngine",
  skin = "purple",
  
  dashboardHeader(title = "ModellerEngine",
                  tags$li(a(href = 'http://www.kantar.com/analyticspractice',
                            img(src = 'Kantar.png',title = "Company Home", height = "20px"),
                            style = "padding-top:10px; padding-bottom:5px;"),class = "dropdown")
                  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebartabs",
    #Acquire side bar menu 
    menuItem("Introduction",tabName = "introduction",icon = icon("cube")),
    menuItem("AutoModeller",tabName = "ols_am",icon = icon("user"),
             menuItem("Acquire", tabName = "Acquire", icon = icon("upload"),
                      menuItem("Variable Console ",tabName = "variableConsole", icon = icon("columns")),
                      menuItem("Direct CSV Upload",tabName = "directCsvUpload", icon = icon("database"))
             ),
             menuItem("Analyse", tabName = "Analyse", icon = icon("th"),
                      menuItem("Data review",tabName = "AM_Data_Review", icon = icon("tasks")),
                      menuItem("Results",tabName = "AM_Result", icon = icon("line-chart")),
                      menuItem("Filter Results",tabName = "AM_FilterResult", icon = icon("filter", lib = "glyphicon"),badgeLabel = "new",badgeColor = "red"),
                      menuItem("Top Models",tabName = "AM_TopResult", icon = icon("line-chart"),badgeLabel = "new",badgeColor = "red")
             )
    ),
    menuItem("OLS Manual",tabName = "ols_manual",icon = icon("user"),
             menuItem("OLSM Acquire", tabName = "olsm_acquire", icon = icon("upload"),
                      menuItem("CSV Upload",tabName = "olsm_csvUp", icon = icon("database"))
             ),
             menuItem("OLSM Analyze", tabName = "olsm_analyze", icon = icon("th"),
                      menuItem("OLSM Model Scope",tabName = "olsm_modelScope",icon = icon("filter", lib = "glyphicon")),
                      menuItem("OLSM Model Manager", tabName = "olsm_modelManager", icon = icon("tasks")),
                      menuItem("OLSM Results",tabName = "olsm_results", icon = icon("line-chart"))
             )
      )
    )
  ),
  
  dashboardBody(useShinyalert(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "http://localhost:1984/default.ico")
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "introduction",
        fluidRow(
          box(width = "12",background = "maroon",h2("Introduction to ModellerEngine_v1.9.2"))
        ),
        fluidRow(
          box(width = "12",solidHeader = T,collapsible = F,
              bsCollapsePanel(title = "OLS AutoModeller",style = "danger",value = h2("AutoModeller"),.values = h5("It can generate exhaustive list of model using all variable combination as per given modelling variable as per the usage in Campaign Watch. It has following features..",br(),
              tags$ol(tags$li("Variable Selection option after uploading data."),
                      tags$li("Bucket Selection option for transformation."),
                      tags$li("AdStock selection option as first or last."),
                      tags$li("Power and S-Shaped transformation options."),
                      tags$li("Modelling Flag option is available to exclude the variable from Model Manager after transformation for modelling."),
                      tags$li("Exclude Model by providing Tstat direction in Model Manager."),
                      tags$li("Bucket wise variable combination option is available."),
                      tags$li("Model Scope Option is given to exclude the data points while modelling."),
                      tags$li("Model Sorting Option is available by model Parameters."),
                      tags$li("Dummy Variable addition option is available over the model with dummy model scope range."),
                      tags$li("Filter models option is available by given variable combination."),
                      tags$li("Top Model filter option is available for each variable combination."),
                      tags$li("Download option is available for Model Result and Normalized Data."),
                      tags$li("Variable Console module which can be use to create variable on the fly."),
                      tags$li("Model Result has Absolute of T-Stat Average and VIF Average with MAPE value of the Model."),
                      tags$li("Bucket can be selected min as zero to create variable combination by bucket."),
                      tags$li("Two Ranking algorithm is available to Ranking the Model which need to select before Running the Regression ie. Nimish Algorithm and Sounava Algorithm."),
                      tags$li("Model Iterations is restricted to 200,000 for 8GB RAM System.")
                      )
              )),
              
              bsCollapsePanel(title = "OLS Manual",style = "danger",value = h2("OLSM"),.values = h5("It can generate single iteration model as per given modelling variable. It has following features..",br(),
                      tags$ol(tags$li("Variable selection option is available for stack and non stack modelling."),
                              tags$li("Geography selection option is available for stacked modelling."),
                              tags$li("Variable Type options available as DepVar, Manual No Trans, Outside No Trans, Fixed Var No Trans, Manual TOF, Outside TOF, Fixed Var TOF, Not in Model."),
                              tags$li("AdStock selection option as first or last."),
                              tags$li("Transformation Type options available as Linear, S-Shaped, Power, Decay (only decay), S-Shaped_Decay_Capped (values is capped to 1)."),
                              tags$li("Non Stack Model using OLS"),
                              tags$li("Stack Model using OLS"),
                              tags$li("Stack Model using WLS"),
                              tags$li("Combined Column feature is available in Model Manager."),
                              tags$li("Fixed Effect feature is available in Model Manager."),
                              tags$li("Download options are available for Model Result, Consolidated All Model Result."),
                              tags$li("Download option is available for full decomposition based for base Model of Non Stack and Stack Model in Unrolled and RolledUp both."),
                              tags$li("Dummy Model feature is available for stack and non stack model."),
                              tags$li("AVM can be downloaded in Unrolled and RolledUp Both as applicable by stack and non stack Model."),
                              tags$li("RolledUp Actual Vs Predicted Plot in available in OLSM for stack Model."),
                              tags$li("In case of MultiCollinearity, Model Variable will not have any value in the Model Result."),
                              tags$li("Mixed Modelling (Fixed + Random over Geography)."),
                              tags$li("Min-Max Adjustment is available."),
                              tags$li("Weighted Mixed Effect Modelling is available."),
                              tags$li("Contribution% for each variable in Model Result."),
                              tags$li("Model Actual, Predicted and Residual data is available below AVM Plot.")
                      )
                ))
          )
        )
      ),
      
      
      tabItem(tabName = "variableConsole",
            fluidRow(
              box(title = "Upload Transform CSV File with Tags to Variable Console",status = "info",width = "7",solidHeader = T,
                  fileInput('file2', label= NULL, accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),buttonLabel = "select CSV file"),
                  actionButton("variableCsvFile","Upload CSV File")
                  )
            ),
            fluidRow(
              box(
                title = "VOF Variable",status = "warning",width = "12",solidHeader = T,
                  tabsetPanel(id = "tabs",
                              tabPanel("Dependent Variable", value = "dep",
                                       wellPanel(
                                         fluidRow(
                                           uiOutput("depInputDisplay")
                                         ),
                                         fluidRow(
                                           wellPanel(
                                             DT::dataTableOutput("deptable"),
                                             actionButton("delete",label = "Delete", icon = icon("Delete")),
                                             downloadButton('download', 'Download as CSV')
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Independent Variable", value = "indep",
                                       wellPanel(
                                         fluidRow(
                                           uiOutput("inDepInputDisplay")
                                         ),
                                         fluidRow(
                                           wellPanel(
                                             DT::dataTableOutput("indeptable"),
                                             actionButton("idelete",label = "Delete", icon = icon("Delete")),
                                             downloadButton('idownload', 'Download as CSV')
                                           )
                                         )
                                       )
                              )
                  )
                  )
            ),
            fluidRow(
              box(title = "Final Dataset",status = "success",width = "12",solidHeader = T,collapsible = F,
                  DT::dataTableOutput('vofTable'),
                  fluidRow(
                    column(2, downloadButton("mdownload", "Download as CSV")),
                    column(4, actionButton("forward_UploadCsv","Continue with AutoModeller",style="color: #212121; background-color: #98fbc9; border-color: #2e6da4"))
                  )
              )
            )
      ),
        
      #AutoModeller Begins    
      tabItem(tabName = "directCsvUpload",
          fluidRow(
            box(title = "Upload data",status = "info",width = "8",solidHeader = T,
              uiOutput('upload')
            ),
            uiOutput("displayVarSelect"),
            uiOutput("amData")
          )
      ),
  
      tabItem(tabName = "AM_Data_Review", uiOutput("displayModelManager")),
      
      tabItem(tabName = "AM_Result",theme = shinytheme("united"),
          navbarPage('Regression Results', id= "inTabset", theme = shinytheme("united"),
                     tabPanel(title = 'Model Result', value = "panel1",
                              #tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("sortAlert",function(message) {eval(message.value);});'))),
                              uiOutput("displayModelResult")
                              ),
                     tabPanel(title = 'Dummy Addition', value = "panel2",
                              uiOutput("DummyModelScopeUI")
                              )
                     )
      ),
  
      
      tabItem(tabName = "AM_FilterResult", uiOutput("displayAM_FilterResult")),
      tabItem(tabName = "AM_TopResult", uiOutput("displayAM_TopResult")),
      
      ##############################################################
      ## OLSM UI Modules ##
      
      tabItem(tabName = "olsm_csvUp",uiOutput("olsm_upload")),
      tabItem(tabName = "olsm_modelScope",uiOutput("olsm_getGeography"),uiOutput("olsm_getModelVar"),uiOutput('olsm_modellingScope')),
      tabItem(tabName = "olsm_modelManager",uiOutput("olsm_displayModelManager")),
      tabItem(tabName = "olsm_results", theme = shinytheme("united"),
              navbarPage('OLSM Regression Results', id= "olsmResultTabset", theme = shinytheme("united"),
                         tabPanel(title = 'Model Result', value = "panel1",
                                  uiOutput("olsmDisplayResult"),uiOutput("olsm_ModelResult")
                         ),
                         tabPanel(title = 'Dummy Addition', value = "panel2",uiOutput("olsmDummyModelScopeUI"),uiOutput("olsmDummyVarTableUI")
                         )
              )
      )
              
    )
  ) 
)
