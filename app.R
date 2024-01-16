library(shiny)
library(shinydashboard)
library(dplyr)
library(reticulate)
library(WebGestaltR)

# source("helper.R",encoding = "UTF-8")
source("helper_func.R",encoding = "UTF-8")




# if (interactive()) {
ui<-dashboardPage(
  
  dashboardHeader(title = em(h3(strong('DDITP'))),
                  titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem(strong("Homepage"), tabName = 'homepage', icon = icon('home')),
                     menuItem(strong('DETP'), tabName = 'datasubmit', icon = icon('arrow-circle-up')),
                     menuItem(strong('DBTP'), tabName = 'datasubmit1', icon = icon('arrow-circle-up')),
                     menuItem(strong('DTMP'), tabName = 'datasubmit2', icon = icon('arrow-circle-up')),
                     menuItem(strong('Results'), tabName = 'dataresults',icon = icon('file')),
                     menuItem(strong('User guide'), tabName = 'userguide', icon = icon('book'))
                   )),
  dashboardBody(
    
    tabItems(    
      tabItem(tabName = 'homepage',
              fluidPage(
                box(
                  # title = h3(strong('Transcriptome-based Multi-scale Network Pharmacological Platform')),
                  width = 200,
                  h1(strong('Drug-direct and -indirect target prediction'), align = "center"),
                  br(),
                  br(),
                  # h2("Specific gene module pair"),
                  p("Target identification is an indispensable part of drug discovery.it is of major concern with drug structurally binding targets.However, relying solely on binding targets is not accurate
                    So we added gene expression data that represent the overall molecular activities of biological samples, It is suitable adaptor to connect activities of drugs and targets."),
                  p("By combining the gene expression profile-based model and the classical binary classification model based on chemical information of drugs and targets, it improves the performance 
                    of inferring drug-direct effect target interactions based on chemical properties. Our work demonstrates that a simple integration gene expression and chemical profiles of drugs and 
                    targets can help discriminate different drug-target interaction types and spotlight drug effect targets.")
                ),
                br(),
                br(),
                box(
                  width = 200,
                  column(
                    
                    width = 4
                  ),
                  column(
                    img(src="figure0.png", 
                        height = 400, width = 600),
                    width = 4
                  )
                ),
                br(),
                br(),
                br(),
                br(),
                box(
                  width = 200,
                  h3(strong('  Contact us')),
                  p('Prof. Li Peng'),
                  p('Address: Shanxi Agricultural University, Taigu 030801, Jinzhong, China'),
                  p('E-mail: lip@sxau.edu.cn')
                )
              )
      ),
      tabItem(tabName = 'datasubmit',
              fluidRow(
                box(
                  title = 'Input',
                  status = 'primary',
                  fileInput("file1", "Upload transcriptional profile data",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  helpText("The upload data should be a csv file of gene list 
                             with metrics (e.g. differntial values), the first column is gene name and the other columns are values"),
                  # width = 10,
                  solidHeader = TRUE
                  
                ),
                
                box(
                  title = 'DETP analysis',
                  status = 'primary',
                  # h3("Multi-scale analysis"),
                  actionButton("Target_gene_action", label = "DETP"),
                  # br(),
                  
                  # br(),
                  
                  #br(),
                  
                  # br(),
                  # actionButton("symptom_action", label = "Symptom"),
                  
                  solidHeader = TRUE
                )
                
              ),
              
              fluidRow( 
                box(
                  title = 'Input data',
                  status = 'primary',
                  tableOutput("data"),
                  width = 10
                )
              )
      ),
      tabItem(tabName = 'datasubmit1',
              fluidRow(
                box(title = "Input",
                    status = "primary",
                    fileInput("file2","Upload smile data",
                              multiple = T,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    helpText("The upload data should be a csv file"),
                    solidHeader =T
                ),
                box(
                  title = 'DBTP analysis',
                  status = 'primary',
                  # h3("Multi-scale analysis"),
                  
                  # br(),
                  
                  # br(),
                  
                  #br(),
                  
                  # br(),
                  # actionButton("symptom_action", label = "Symptom"),
                  
                  
                  #br(),
                  
                  actionButton("smile_action",label = "DBTP"),
                  solidHeader = TRUE
                ),
                
              ),
              
              fluidRow( 
                box(
                  title = 'Input data',
                  status = 'primary',
                  tableOutput("data1"),
                  width = 16
                )
              )
      ),
      tabItem(tabName = 'datasubmit2',
              fluidRow(
                box(title = "Input",
                    status = "primary",
                    fileInput("file3","Upload effective target",
                              multiple = T,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    helpText("The upload data should be a csv file"),
                    solidHeader =T
                ),
                box(
                  title = 'DTMP analysis',
                  status = 'primary',
                  
                  actionButton("path_analyse",label = "DTMP"),
                  solidHeader = TRUE
                ),
              ),
              fluidRow( 
                box(title = "Input",
                    status = "primary",
                    fileInput("file4","Upload Binding Targets",
                              multiple = T,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    helpText("The upload data should be a csv file"),
                    solidHeader =T
                )
              )
      ),
      tabItem(tabName = 'dataresults',
              fluidPage(
                tabsetPanel(
                  
                  tabPanel("DETP_Target gene",
                           box(
                             width = 200,
                             downloadButton("downloadData1", "Download")
                           ),
                           box(
                             width = 200,
                             tableOutput("targetdata")
                           )
                  ),
                  
                  tabPanel("DBTP Result",
                           box(width = 200,
                               downloadButton("downloadData", "Download")),
                           box(width = 200,
                               tableOutput(outputId = "combinedata")
                           )),
                  tabPanel("DTMP Result",
                           box(width = 200,
                               downloadButton("downloadData2", "Download")),
                           box(width = 200,div(style = "overflow-y: scroll; height: 700px;",
                                               tableOutput(outputId = "pathway_link")
                           )
                           ))        
                  
                  
                  
                  
                  
                )
              )
              
              # box(
              #   title = h3(strong('Transcriptome-based Multi-scale Network Pharmacological Platform')),
              #   width = 200,
              # 
              #   tableOutput("resultdata")
              #  
              # ),
              # box(
              #   title = h3(strong('Transcriptome-based Multi-scale Network Pharmacological Platform')),
              #   width = 200,
              #   
              #   tableOutput("resultdata")
              #   
              # )
              # )
      ),
      tabItem(tabName = 'userguide',
              
              fluidPage(
                box(
                  width = 200,
                  h1(strong("User guide"), align = "center"),
                  
                  h2("An introduction to DDITP(Drug-direct and -indirect target prediction)."),
                  
                  p("DDITP, as the combination of DETP and DBTP, evaluates drug-target interactions by calculating the drug direct-target score (DDS), 
                  which is the linear combination of drug effect-target score and drug binding target probability, determined by the binary logistic regression.
                  Moreover, by combining the gene expression profile-based model and the classical binary classification model based on chemical information of drugs and targets, 
                  it improves the performance of inferring drug-direct effect target interactions based on chemical properties."),
                  
                  
                  h2("Drug-effect target prediction based on gene expression data (DETP)."),
                  
                  p("DETP evaluates drug-target interactions by calculating the drug effect target scores (DES)."),
                  
                  h3("When we use the features of DETP, we should upload a file format consistent with the following images."),
                  
                  tags$img(src = "example.png", height = "500px", width = "300px"),
                  
                  
                  h2("Drug-binding target prediction based on chemical properties (DBTP)."),
                  
                  
                  
                  p("DBTP is a classical binary classification model trained in the BindingDB dataset by using the DeepPurpose framework and can predict the binding probabilities between drugs and targets. 
                DDITP evaluates drug-direct effect target associations by the drug direct-target score (DDS) which is the linear combination of DES and drug binding target probability, 
                  determined by the binary logistic regression."),
                  
                  h3("when we use the features of DBTP, we should upload a file format consistent with the following images.
                   The web page will return some values, all values can be downloaded"),
                  
                  img(src = "example2.png", height = 300, width = 600),
                  
                  h2("Drug target mapping on signaling pathways.(DTMP)"),
                  
                  p("DTMP is Drug target mapping on signaling pathways. Users need to upload the effect target of DETP analysis and the combination target of DBTP analysis.Then according to the requirements of screening, to get the expected effect targets and structural targets, and then the effect targets and combination

The targets are respectively uploaded as shown in the figure below, click the DTMP button for analysis, and then the website will return the enriched effect targets

Path and return to the direct target and indirect target in the path, the user can click the specific link returned by the website to understand the path details."),
                  
                  h3("When we use the features of DTMP, we should upload a file format consistent with the following images."),
                  
                  img(src = "example3.png", height = 300, width = 600)
                  
                  
                )
              )
              
      )
      
    )
    
  ),
  
  skin = 'blue',
  #jindutiao weizhi
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(30%);;
           left: calc(30%);;
}"
      )
    )
  )
  
) 


server<-function(input, output,session) {
  datainput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, row.names = 1) 
    
  })
  
  
  output$data <- renderTable({
    
    return(head(datainput(),50))
    
  })
  
  datainput1 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, row.names = NULL) 
    
  })
  
  output$data1 <- renderTable({
    
    return(head(datainput1(),50))
    
  })
  datainput2 <- reactive({
    req(input$file3)
    read.csv(input$file3$datapath, header = TRUE) 
    
  })
  
  datainput3 <- reactive({
    req(input$file4)
    read.csv(input$file4$datapath, header = TRUE) 
    
  })
  #   observe({
  #     file <- input$file2
  #     filepath <- "./DETP/input_data/DH_compound_smile.csv"
  #     if (!is.null(file)) {
  #       # 保存文件
  #       write.csv(read.csv(file$datapath), file = filepath, row.names =T)
  #     }
  #   })
  
  
  observeEvent(input$Target_gene_action,{
    progress <- 
      Progress$new(
        session,
        min = 1,
        max = 10
        # ,
        # style = "old"
      )
    on.exit(progress$close())
    progress$set(
      message = 'Calculation in progress, this may take some minutes,you can jump to "result page" for waiting...'
    )
    for(i in 1:5){
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    dataoutput <- fun_target(querydata=datainput())
    
    #rendertext only output the last text value of the list.
    #renderui can output all list contents, but need to br() for linefeed
    
    
    output$targetdata <- renderTable({
      return(dataoutput[[1]])
    })
    
    for(i in 6:10){
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    output$downloadData1 <- downloadHandler(
      filename = "DH_effect_target.csv",
      content = function(file) {
        write.csv(dataoutput[[1]], file, row.names = FALSE)
      }
    )
    
    # dataoutput <- fun_target(querydata=datainput())
    
    
  })
  
  observeEvent(input$path_analyse,{
    progress <- 
      Progress$new(
        session,
        min = 1,
        max = 10
        # ,
        # style = "old"
      )
    on.exit(progress$close())
    progress$set(
      message = 'Calculation in progress, this may take some minutes,you can jump to "result page" for waiting...'
    )
    for(i in 1:5){
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    values <- as.character(unlist(datainput2())) #interestGene的R对象
    common_values <- intersect(datainput2()[,1], datainput3()[,1]) #直接靶点
    #rendertext only output the last text value of the list.
    #renderui can output all list contents, but need to br() for linefeed
    effective_remaining <- datainput2()[!(datainput2()[,1] %in% common_values), ]  #间接靶点
    
    enrichResult <- WebGestaltR(enrichMethod="ORA", organism="hsapiens",
                                enrichDatabase="pathway_KEGG", interestGene = values,
                                interestGeneType="genesymbol", referenceSet = "genome_protein-coding",
                                isOutput=FALSE,fdrThr = 0.05) #富集分析
    split_enrich = list()  #将富集到的基因分开
    enrich_direct = list()  #富集到的直接靶点
    enrich_indirect = list()  #富集到的间接靶点
    for (i in 1:length(enrichResult[,1])){
      split_enrich[[i]] = strsplit(enrichResult$userId[i], ";")[[1]]
      enrich_direct[[i]] = split_enrich[[i]][split_enrich[[i]] %in% common_values]
      enrich_indirect[[i]] = split_enrich[[i]][split_enrich[[i]] %in% effective_remaining]
    }
    Direct_target_set=c() #直接靶点集合
    InDirect_target_set=c() #间接靶点集合
    for(i in 1:length(enrichResult[,1])){
      Direct_target_set[i] = paste(enrich_direct[[i]],collapse = ";")
      InDirect_target_set[i] = paste(enrich_indirect[[i]],collapse = ";")
    }
    enrichResult$direct_target <- Direct_target_set   
    enrichResult$indirect_target <- InDirect_target_set
    output$pathway_link <- renderTable({
      return(enrichResult)
    })
    
    for(i in 6:10){
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    output$downloadData2 <- downloadHandler(
      filename = "DTMP.csv",
      content = function(file) {
        write.csv(enrichResult, file, row.names = FALSE)
      }
    )
    
    # dataoutput <- fun_target(querydata=datainput())
    
    
  })  
  observeEvent(input$smile_action,{
    progress<-Progress$new(
      session,
      min = 1,
      max=10
    )
    on.exit(progress$close())
    progress$set(
      message='Calculation in progress, this may take some minutes,Please wait patiently...'
    )
    for(i in 1:5){
      progress$set(value=i)
      Sys.sleep(0.5)
    }
    py_data <- r_to_py(datainput1())
    py$py_data <- py_data
    source_python("DT_predict.py")
    # file_path <- "./result/DH_direct_target.csv"
    # data <- read.csv(file_path)
    # columns <- c("Binding_score", "SMILES", "target_names")
    # data <- data %>% select(all_of(columns))
    predict_result <- py$y_predict_result
    output$combinedata <- renderTable(head(predict_result),50)
    for(i in 6:10){
      progress$set(value=i)
      Sys.sleep(0.5)
    }
    output$downloadData <- downloadHandler(
      filename = "DH_direct_target.csv",
      content = function(file) {
        write.csv(predict_result, file, row.names = FALSE)
      }
    )
  })
}
# }
# Create Shiny app ----
shinyApp(ui, server)
