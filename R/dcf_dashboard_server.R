#'@name dcf_dashboard_server
#'@export
dcf_dashboard_server <- function(input, output,session,
                                 tasks, reporting_entity, ref_reporting_entities,
                                 allow_download=FALSE,
                                 gisviewer_url = NULL) {
  
  tasks_obj<-tasks
  
  tasks<-unlist(lapply(tasks, function(x){setNames(x$id,x$label)}))
  
  waiting_screen<-tagList(
    h3("Initialisation of Application"),
    waiter::spin_flower()
  )
  
  waiter::waiter_show(html = waiting_screen)
  
  #datasets
  #---------------------------------------------------------------------------------------
  dt_reporting_entities<-readr::read_csv(ref_reporting_entities)
  
  #db
  #---------------------------------------------------------------------------------------
  cat("DB connection parameters\n")
  conn_args <- config::get("dataconnection")
  cat(paste0("DB_DRV: ", conn_args$drv, "\n"))
  cat(paste0("DB_HOST: ", conn_args$host, "\n"))
  cat(paste0("DB_PORT: ", conn_args$port, "\n"))
  cat(paste0("DB_DBNAME: ", conn_args$dbname, "\n"))
  cat(paste0("DB_USER: ", conn_args$user, "\n"))
  if(conn_args$password!="") cat(paste0("DB_PASSWORD: **********\n"))
  conn_start = Sys.time()
  cat(paste0("Before connection: ",format(conn_start,"%y%m%d %H:%M:%S"),"\n"))
  pool <- pool::dbPool(
    drv = DBI::dbDriver(conn_args$drv),
    dbname = conn_args$dbname,
    host = conn_args$host,
    port = conn_args$port,
    user = conn_args$user,
    password = conn_args$password
  )
  end_start = Sys.time()
  cat(paste0("After connection:", format(end_start,"%y%m%d %H:%M:%S"),"\n"))
  cat(paste0("Difference: ", as.character(end_start-conn_start), " s\n"))
  
  
  
  data_tasks<-lapply(setNames(tasks,tasks),function(x){
    getDataTaskDBData(pool, x)
  })
  
  data_tasks = data_tasks[!sapply(data_tasks, is.null)]
  
  reporting_entities<-dt_reporting_entities$code
  
  data<-reactiveVal(NULL)
  data_s<-reactiveVal(NULL)
  gap<-reactiveVal(5)
  heatmap_size<-reactiveVal(12)
  dataAvailable<-reactiveVal(ifelse(length(data_tasks)==0,FALSE,TRUE))
  
  waiter_hide()
  
  output$indicators<-renderUI({
    div( class="row",
         div(class = "col-xs-3 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox(names(reporting_entity),sprintf("%s / %s",length(getUniqueValues(data_tasks,tasks,reporting_entity)),length(reporting_entities)), icon = icon("flag"), fill = TRUE,color="blue",width = NULL)),
         div(class = "col-xs-3 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Tasks",length(data_tasks), icon = icon("list-check"), fill = TRUE,color="yellow",width = NULL)),
         div(class = "col-xs-3 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Period",sprintf("%s-%s",min(year(getUniqueValues(data_tasks,tasks,"time_start"))),max(year(getUniqueValues(data_tasks,tasks,"time_end")))), icon = icon("clock"), fill = TRUE,color="green",width = NULL)),
         div(class = "col-xs-3 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Species",length(getUniqueValues(data_tasks,tasks,"species")), icon = icon("fish"), fill = TRUE,color="aqua",width = NULL))
    )
  })
  
  observeEvent(input$dimension,{
    
    print(sprintf("WINDOW WIDTH : %s ; WINDOW HEIGHT : %s",input$dimension[1],input$dimension[2]))
    
    newgap<-ifelse(input$dimension[1]>2000,10,
                   ifelse(input$dimension[1]>1000,5,
                          ifelse(input$dimension[1]>600,3,
                                 1)))
    gap<-gap(newgap)
    
    newsize<-ifelse(input$dimension[1]>2000,14,
                    ifelse(input$dimension[1]>1000,12,
                           ifelse(input$dimension[1]>600,8,
                                  6)))
    
    heatmap_size<-heatmap_size(newsize)
  })
  
  output$summary_content<-renderUI({
    tagList(
      fluidRow(
        div(class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
            uiOutput("entities_selector_s")
        ),
        div(
          class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("stat_selector_s")
        ),
        div(
          class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("download_wrapper")
        )
      ),
      uiOutput("heatmap_s_legend"),
      uiOutput("heatmap_s_wrapper")
      
    )
  })
  
  output$by_task_content<-renderUI({
    if(dataAvailable()){
      tagList(
        fluidRow(
          div(class = "col-sm-6 col-md-6 col-lg-2 col-xl-2",
              uiOutput("task_selector"),
              uiOutput("entities_selector")
          ),
          div(
            class = "col-sm-6 col-md-6 col-lg-4 col-xl-4",
            uiOutput("download_task_wrapper")
        )
        ),
        fluidRow(
          uiOutput("heatmap_legend"),
          withSpinner(plotlyOutput("heatmap"),type=4)
        )
      )}else{
        p("(No data available)")
      }
  })
  
  output$template_content<-renderUI({
    tagList(
      fluidRow(
        div(class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
            uiOutput("task_selector_template")
        ),
        div(
          class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("format_template_wrapper")
        ),
        div(
          class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("with_aliases_template_wrapper")
        ),
        div(
          class = "col-xs-4 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("download_template_btn_wrapper")
        )
    )
    )
  })
  
  output$with_aliases_template_wrapper<-renderUI(({
    req(input$task_template)
    req(input$format_template)
    checkboxInput("with_aliases_template", "Use column aliases names", value = FALSE)
  }))
  
  output$menu<-renderUI({
    tagList(
      uiOutput("indicators"),
      div(class = "col-md-12",
          tabBox(id = "tabbox",title=NULL,height="600px",width = "100%",
                 tabPanel(title=tagList(icon("clipboard")," Summary"),
                          value="tab_summary",
                          if(reporting_entity == "flagstate"){
                            tabsetPanel(id = "summary_tabbox", title = NULL, height="600px",width = "100%", type = "pills",
                              tabPanel(title=tagList(icon("table"), " Table"),
                                       value = "tab_summary_table",
                                       uiOutput("summary_content")),
                              tabPanel(title=tagList(icon("map"), " Map"),
                                       value = "tab_summary_map",
                                       "TODO")
                            )
                          }else{
                            uiOutput("summary_content")
                          }
                 ),
                 tabPanel(title=tagList(icon("list")," By Task"),
                          value="tab_by_task",
                          uiOutput("by_task_content")
                 ),
                 if(!is.null(gisviewer_url)){
                   tabPanel(title=tagList(icon("map")," GIS Viewer"),
                          value="tab_map",
                          shiny::htmlOutput("gisviewer_frame")
                  )
                 }else{NULL},
                 tabPanel(title=tagList(icon("file")," Data templates"),
                          value="tab_template",
                          uiOutput("template_content")
                 ),
          )
      )
    )
  })
  
  
  
  #status selector
  output$entities_selector <- renderUI({
    checkboxInput("limit_entities", "Limit to entities with data", TRUE)
  })
  
  output$entities_selector_s <- renderUI({
    checkboxInput("limit_entities_s", "Limit to entities with data", dataAvailable())
  })
  
  output$stat_selector_s<-renderUI({
    selectizeInput("stat_s",
                   label="Statistic",
                   multiple = F,
                   choices = c("Oldest available year"="min_year",
                               "Most recent available year"="max_year",
                               "Covered period"="period",
                               "Available years"="available_years",
                               "Number of years"="nb_year",
                               "Number of records"="nb_record"),
                   selected="period"
    )
  })
  
  #Task selector
  output$task_selector<-renderUI({
    selectizeInput("task",
                   label="Task",
                   multiple = F,
                   choices = tasks,
                   selected=NULL,
                   options = list(
                     placeholder = "Please select a task",
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  output$task_selector_template<-renderUI({
    selectizeInput("task_template",
                   label="Task",
                   multiple = F,
                   choices = tasks,
                   selected=NULL,
                   options = list(
                     placeholder = "Please select a task",
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  output$format_template_wrapper<-renderUI({
      req(input$task_template)
      
      selected_task<-tasks_obj[[which(sapply(tasks_obj, function(x) x$id == input$task_template))]]
      formats<-unlist(lapply(selected_task$formats, function(x){setNames(x$id,x$label)}))
      
          div(
            if(length(formats)>1){
              selectizeInput("format_template",
                             label="Select format",
                             multiple = F,
                             choices = formats,
                             selected=formats[1],
                             options = list(
                               placeholder = "Please select a format",
                               onInitialize = I('function() { this.setValue(""); }')
                             )
              )
            }else{
              disabled(
                selectizeInput("format_template",
                               label="Select format",
                               multiple = F,
                               choices = formats,
                               selected=formats[1]
                )
              )
            }
        )
  })
  
  output$download_template_btn_wrapper<-renderUI({
    req(input$task_template)
    req(input$format_template)
    
    if(input$format_template==""){
      disabled(downloadButton("download_template",label="Download template",icon=shiny::icon("download"),style = "padding: 5px 20px; margin: 2px 8px;"))
    }else{
      
      downloadButton("download_template",label="Download template",icon=shiny::icon("download"),style = "padding: 5px 20px; margin: 2px 8px;")
    }
  })
  
  output$download_template <- downloadHandler(
    filename = function() { 
      sprintf("template_%s_%s.zip",input$task_template,input$format_template)
    },
    content = function(filename) {
      selected_task<-tasks_obj[[which(sapply(tasks_obj, function(x) x$id == input$task_template))]]
      format_ref<-selected_task$formats[[which(sapply(selected_task$formats, function(x) x$id ==input$format_template))]]
      
      format_ref<-format_ref$ref
      format_spec <- vrule::format_spec$new(json = jsonlite::read_json(format_ref))
      zipfile_name <- format_spec$createTemplate(use_alias=input$with_aliases_template)
      file.copy(zipfile_name, filename)
    },
    contentType = "application/zip")
  
  observeEvent(data_tasks,{
    if(dataAvailable()){
      
      summary<-do.call("rbind",lapply(tasks,function(x){
        data_task<-data_tasks[[x]]
        if(!is.null(data_task)){
          colnames(data_task)[colnames(data_task)==reporting_entity] = "reporting_entity"
          data_task<-data_task%>%
            dplyr::group_by(reporting_entity)%>%
            dplyr::summarise(period=paste0("first:",year(min(time_end,na.rm=T)),"- last:",year(max(time_end,na.rm=T))),
                      min_year=as.character(year(min(time_end,na.rm=T))),
                      max_year=as.character(year(max(time_end,na.rm=T))),
                      nb_year=as.character(length(unique(year(time_end)))),
                      nb_record=as.character(length(reporting_entity)),
                      available_years=paste0(pretty_seq(sort(unique(year(time_end)))),collapse=";"))%>%
            dplyr::arrange(desc(reporting_entity))%>%
            dplyr::ungroup()%>%
            dplyr::mutate(task=gsub(": ",":\n",names(tasks[tasks==x])))
        }
      })
      )
      
    }else{
      summary<-do.call("rbind",lapply(tasks,function(x){
        
        data_task<-data.frame(reporting_entity="",
                              period="(no data)",
                              min_year="(no data)",
                              max_year="(no data)",
                              nb_year="0",
                              nb_record="0",
                              available_years="(no data)",
                              task=x)%>%
          dplyr::arrange(desc(reporting_entity))%>%
          dplyr::ungroup()
        colnames(data_task)[colnames(data_task)=="reporting_entity"] = reporting_entity
      })
      )
      
    }
    data_s<-data_s(summary)
  })
  
  output$download_wrapper<-renderUI({
    req(data_s())
    if(nrow(data_s())>0){
      downloadButton("download",label="Download summary",icon=shiny::icon("file-excel"),style = "background: #0d6cac !important;  padding: 5px 20px!important; margin: 2px 8px; color: #fff !important; border-radius: 0.25rem; border: 0;")
    }
  })
  
  output$download_task_wrapper<-renderUI({
    if(allow_download==T){
    req(data())
    req(input$task)
    
    selected_task<-tasks_obj[[which(sapply(tasks_obj, function(x) x$id == input$task))]]
    formats<-unlist(lapply(selected_task$formats, function(x){setNames(x$id,x$label)}))
    
    if(nrow(data())>0){
      box(
        div(
          if(length(formats)>1){
            selectizeInput("format",
                           label="Download dataset",
                           multiple = F,
                           choices = formats,
                           selected=formats[1],
                           options = list(
                             placeholder = "Please select an export format",
                             onInitialize = I('function() { this.setValue(""); }')
                           )
            )
          }else{
            disabled(
              selectizeInput("format",
                             label="Export format",
                             multiple = F,
                             choices = formats,
                             selected=formats[1]
              )
            )
          },
          checkboxInput("with_codelist_labels", "Enrich with codelists labels", value = FALSE),
          checkboxInput("with_col_aliases", "Use column aliases names", value = FALSE),
          uiOutput("download_task_btn_wrapper")
          
        )
      )
    }
    }else{
      NULL
    }
  })
  
  output$download_task_btn_wrapper<-renderUI(
    if(input$format==""){
      disabled(downloadButton("download_task",label="Download data",icon=shiny::icon("download"),style = "padding: 5px 20px; margin: 2px 8px;"))
    }else{

      downloadButton("download_task",label="Download data",icon=shiny::icon("download"),style = "padding: 5px 20px; margin: 2px 8px;")
    }
  )
  
  output$download_task <- downloadHandler(
    filename = function() { 
      sprintf("data_%s_%s.csv",input$task,Sys.Date())
    },
    content = function(filename) {
      req(nrow(data())>0)
      req(!is.null(input$with_col_aliases))
      req(!is.null(input$with_codelist_labels))
      req(!is.null(input$format))
      
      #inherit dataset
      target_data<-data()
      
      #inherit format 
      selected_task<-tasks_obj[[which(sapply(tasks_obj, function(x) x$id == input$task))]]
      format_ref<-selected_task$formats[[which(sapply(selected_task$formats, function(x) x$id ==input$format))]]
      
      format_ref<-format_ref$ref
      format_spec <- vrule::format_spec$new(json = jsonlite::read_json(format_ref))
      
      if(input$format!="generic"){
        print("Transforming to 'simplified' format")
        target_data<-genericToSimplified(target_data)
        print("Successful transformation from 'generic' to 'simplified' format!")
      }
      if(input$with_codelist_labels){
        #enrich with codelist labels
        for(col in  names(target_data)){
          column_spec = format_spec$getColumnSpecByName(col)
          if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("month")
          if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("quarter")
          if(!is.null(column_spec)){
            if(column_spec$hasCodelist() && any(sapply(column_spec$rules, is, "vrule_codelist"))){
              cl_rule = column_spec$rules[sapply(column_spec$rules, is, "vrule_codelist")][[1]]
              ref<-subset(cl_rule$ref_data,select=c(code,label))
              #manage column aliases if checked
              alias = col
              if(input$with_col_aliases) if(length(column_spec$aliases)>0) alias = column_spec$aliases[[1]]
              names(ref)<-c(sprintf("%s [code]",alias),sprintf("%s [label]",alias))
              names(target_data)[names(target_data) == col] <-sprintf("%s [code]",alias)
              target_data<-base::merge(target_data,ref)
            }else{
              #manage column aliases if checked
              if(input$with_col_aliases) if(length(column_spec$aliases)>0) {
                names(target_data)[names(target_data) == col] <- column_spec$aliases[[1]]
              }
            }
          }
        }
      }else{
        for(col in  names(target_data)){
          column_spec = format_spec$getColumnSpecByName(col)
          if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("month")
          if(is.null(column_spec)) column_spec = format_spec$getColumnSpecByName("quarter")
          if(!is.null(column_spec)){
            #manage column aliases if checked
            if(input$with_col_aliases) if(length(column_spec$aliases)>0) {
              names(target_data)[names(target_data) == col] <- column_spec$aliases[[1]]
            }
          }
        }
      }
      
      #reorder
      target_data = do.call("cbind", lapply(format_spec$column_specs, function(column_spec){
        out = NULL
        col = column_spec$name
        if(column_spec$name %in% c("month", "quarter")) col = "period"
        if(input$with_col_aliases) if(length(column_spec$aliases)>0){
          col = column_spec$aliases[[1]]
        }
        has_cl = column_spec$hasCodelist() && any(sapply(column_spec$rules, is, "vrule_codelist"))
        reorder = if(has_cl){
          startsWith(colnames(target_data), col) #if enrichable with labels, we should get the code/label columns
        }else{
          colnames(target_data) == col #if not enrichable with labels, we look for identity
        }
          
        if(length(reorder)>0){
          columns = colnames(target_data)[reorder]
          out = target_data[,columns]
          if(length(columns)==1){
            out = data.frame(col = out)
            colnames(out) = columns
          }
        }
        if(column_spec$name == "month") colnames(out)[colnames(out)=="period"] = "month"
        if(column_spec$name == "quarter") colnames(out)[colnames(out)=="period"] = "quarter"
        out
      }))
      print("Successful data export!")
      readr::write_csv(target_data,file = filename)
    })
  
  output$download <- downloadHandler(
    filename = function() { 
      sprintf("summary.xlsx")
    },
    content = function(filename) {
      req(nrow(data_s())>0)
      wb = createWorkbook()
      
      for(i in c("period","available_years","min_year","max_year","nb_year","nb_record")){
        df<-data_s()
        colnames(df)[colnames(df)==reporting_entity] = "reporting_entity"
        df<-df[,c("reporting_entity","task",i)]
        
        names(df)[names(df) == i] <- "stat"
        df<-unique(df)
        
        entity_list <- NULL
        if(isTRUE(input$limit_entities_s)){
          entity_list<-unique(df$reporting_entity)
        }else{
          entity_list<-reporting_entities
        }
        
        df<-df%>%
          dplyr::rowwise()%>%
          dplyr::mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
          dplyr::ungroup()%>%
          tidyr::complete(nesting(task),reporting_entity=entity_list,fill = list(value=0,stat="(no data)"))%>%
          dplyr::arrange(desc(reporting_entity))
        
        df_values<-df$value
        
        df<-df%>%
          select(-value)%>%
          pivot_wider(names_from = task,values_from = stat,names_sort=T)%>%
          filter(reporting_entity %in% entity_list)%>%
          rename(` `=reporting_entity)
        
        addWorksheet(wb, i)
        setColWidths(wb, i, cols = 1:ncol(df), widths = "auto")
        nodataStyle <- createStyle(fontColour = "black", bgFill = "gray")
        dataStyle <- createStyle(fontColour = "black", bgFill = "#45AD15")
        conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "contains", rule = "(no data)", style = nodataStyle)
        conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "notcontains", rule = "(no data)", style = dataStyle)
        writeData(wb, sheet = i, x = df, startCol = 1)
        
      }
      saveWorkbook(wb, filename, overwrite = TRUE)
      
    })
  
  observeEvent(input$task,{
    req(input$task)
    if(!is.null(input$task)|input$task!=""){
      file<-data_tasks[[input$task]]
      data<-data(file)
    }
  })
  
  output$heatmap<-renderPlotly({
    req(!is.null(data()))
    req(!is.null(input$limit_entities))
    df<-data()
    colnames(df)[colnames(df)==reporting_entity] <- "reporting_entity"
    df<-subset(df,select=c(reporting_entity,time_end))
    df$time_end<-year(df$time_end)
    df<-unique(df)
    df$value<-1
    
    entity_list <- NULL
    if(isTRUE(input$limit_entities)){
      entity_list<-unique(df$reporting_entity)
    }else{
      entity_list<-reporting_entities
    }
    
    df<-df%>%
      tidyr::complete(nesting(time_end=full_seq(time_end, 1)),reporting_entity=entity_list,fill = list(value=0))%>%
      dplyr::rename(year=time_end)%>%
      dplyr::arrange(desc(reporting_entity),year)%>%
      dplyr::filter(reporting_entity %in% entity_list)%>%
      tidyr::pivot_wider(names_from = year,values_from = value,names_sort=T)
    
    y_lab<-df$reporting_entity
    x_lab<-colnames(df)[-1]
    
    df_matrix<-as.matrix(df[,-1])
    
    colorScale <- data.frame(
      z = c(0,1),
      col=c("grey","green")
    ) 
    
    color_s <- setNames(data.frame(c(0,1),c("grey","green") ), NULL)
    
    fig<-plot_ly(
      height = 150+40*length(y_lab),
      x=x_lab,
      y=y_lab,
      z = df_matrix,
      zmin=0,
      zmax=1,
      xgap=gap(),
      ygap=gap(),
      colorscale = color_s,
      colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("missing","available"),len=0.2), 
      type = "heatmap",
      showscale = FALSE 
      
    )
    
    fig<-plotly::layout(fig,
                showlegend = FALSE,
                xaxis = list(tickvals=seq(min(x_lab),max(x_lab),1),ticktext=as.character(seq(min(x_lab),max(x_lab),1)),showgrid = F)
    )
    return(fig)
  })
  
  output$heatmap_s<-renderPlotly({
    req(!is.null(data_s()))
    req(!is.null(input$limit_entities_s))
    req(dataAvailable()|(!dataAvailable()&!input$limit_entities_s))
    req(!is.null(input$stat_s))
    df<-data_s()
    colnames(df)[colnames(df)==reporting_entity] = "reporting_entity"
    df<-df[,c("reporting_entity","task",input$stat_s)]
    
    names(df)[names(df) == input$stat_s] <- "stat"
    df<-unique(df)
    
    df<-df%>%
      rowwise()%>%
      mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
      ungroup()
    
    max_value<-max(df$value,na.rm=T)
    entity_list <- NULL
    if(isTRUE(input$limit_entities_s)){
      entity_list<-unique(df$reporting_entity)
    }else{
      entity_list<-reporting_entities
    }
    
    df<-df%>%
      tidyr::complete(nesting(task),reporting_entity=entity_list,fill = list(value=0,stat="(no data)"))%>%
      dplyr::arrange(desc(reporting_entity))%>%
      dplyr::filter(reporting_entity %in% entity_list)
    
    text<-df%>%
      dplyr::select(-value)
    
    dfm<-df%>%
      dplyr::select(-stat)%>%
      tidyr::pivot_wider(names_from = task,values_from = value,names_sort=T)
    
    y_lab<-dfm$reporting_entity
    x_lab<-colnames(dfm)[-1]
    
    df_matrix<-as.matrix(dfm[,-1])
    
    # colorScale <- data.frame(
    #   z = c(0,1),
    #   col=c("grey","green")
    # ) 
    
    if(input$stat_s%in%c("nb_year","nb_record")){
      fig<-plot_ly(
        height = 150+40*length(y_lab),
        x=x_lab,
        y=y_lab,
        z = df_matrix,
        zmin=0,
        zmax=max(as.numeric(text$stat)),
        xgap=gap(),
        ygap=gap(),
        color = df$value,
        colors = c("grey", "green"),
        type = "heatmap",
        showscale = FALSE,
        textposition = 'inside'
      )
      
    }else{
      
      fig<-plot_ly(
        height = 150+40*length(y_lab),
        x=x_lab,
        y=y_lab,
        z = df_matrix,
        zmin=0,
        zmax=1,
        xgap=gap(),
        ygap=gap(),
        colorscale = setNames(data.frame(c(0,1),c("grey","green") ), NULL),
        colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("no data","data"),len=0.2), 
        type = "heatmap",
        showscale = FALSE,
        textposition='inside'
      )
      
    }
    
    fig<-plotly::layout(fig,
                showlegend = FALSE,
                uniformtext=list(minsize=0, mode='show'),
                xaxis = list(side ="top",showgrid = F)
    )%>%add_annotations(x = text$task, y = text$reporting_entity, text = text$stat, xref = 'x', yref = 'y', showarrow = FALSE, font=list(size=heatmap_size(),color='black'))
    return(fig)
  })
  output$nodata_s<-renderUI({
    req(!is.null(data_s()))
    req(!is.null(input$limit_entities_s))
    req(!dataAvailable()&input$limit_entities_s)
    p("(no data available)")
  })
  
  output$heatmap_s_wrapper<-renderUI({
    if(!dataAvailable()&isTRUE(input$limit_entities_s)){
      uiOutput("nodata_s")
    }else{
      fluidRow(
        withSpinner(plotlyOutput("heatmap_s"),type=4)
      )
    }
  })
  
  output$heatmap_s_legend<-renderUI({
    fluidRow(HTML("<i class='fa-solid fa-circle' style='color: green;'></i>&nbsp;data&emsp;&emsp;<i class='fa-solid fa-circle' style='color: grey;'></i>&nbsp;no data"),align="center")
  })
  
  output$heatmap_legend<-renderUI({
    req(!is.null(data()))
    req(!is.null(input$limit_entities))
    fluidRow(HTML("<i class='fa-solid fa-circle' style='color: green;'></i>&nbsp;available&emsp;&emsp;<i class='fa-solid fa-circle' style='color: grey;'></i>&nbsp;missing"),align="center")
  })
  
  output$gisviewer_frame <- renderUI({
    req(!is.null(gisviewer_url))
    my_iframe <- div(
      tags$a("Open map viewer in a new tab", href = gisviewer_url, target = "_blank"), tags$br(),
      tags$iframe(src = gisviewer_url, height="700px", width="100%")
    )
    my_iframe
  })
  
  #session$allowReconnect(TRUE)
}
