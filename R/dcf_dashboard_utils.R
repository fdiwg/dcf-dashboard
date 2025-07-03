#'@name getDataTaskTablename
#'@export
getDataTaskTablename <- function(task_id){
  table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
  return(table_id)
}

#'@name getDataTaskDBData
#'@export
getDataTaskDBData <- function(pool, task_id){
  out <- NULL
  table_id <- getDataTaskTablename(task_id)
  hasData <- DBI::dbExistsTable(pool, table_id, schema = "public")
  if(hasData){
    out <- DBI::dbReadTable(pool, table_id, schema = "public")
  }
  return(out)
}

#'@name getUniqueValues
#'@export
getUniqueValues<-function(data,tasks,target){
  summary<-do.call("c",lapply(tasks,function(x){
    task<-data[[x]]
    if(target%in%names(task)){
      unique(task[[target]])
    }else{
      c()
    }}))
  summary<-unname(summary)
  summary<-unique(summary)
  summary[!is.na(summary)]
}

#'@name pretty_seq
#'@export
pretty_seq<-function(x){
  
  break_list<-which(c(1,diff(x)) != 1)
  done<-c()
  new_vec<-c()
  
  if(length(break_list>0)){
    for(i in break_list){
      
      target<-x[1:i-1]
      
      target<-setdiff(target,done)
      
      done<-c(done,target)
      
      min_v<-min(target)
      
      max_v<-max(target)
      
      seq_v<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
      
      new_vec<-c(new_vec,seq_v)
    }
    
    remaining<-setdiff(x,done)
    new_vec<-c(new_vec,remaining)
  }else{
    min_v<-min(x)
    max_v<-max(x)
    new_vec<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
  }
  return(new_vec)
}

#'@name genericToSimplified
#'@export
genericToSimplified <- function(file){
  
  
  #read data in case it's not already done
  data <- file
  if(!is.data.frame(file)) data <- readDataFile(file)
  
  new_data<-data%>%
    mutate(combine=paste0(measurement,"_",measurement_type))%>%
    select(-measurement,-measurement_type)%>%
    mutate(measurement_value=as.character(measurement_value))%>%
    pivot_longer(c(measurement_unit,measurement_value),names_to="tempo",values_to="measurement_cat")%>%
    mutate(combine=ifelse(endsWith(tempo,"_unit"),paste0(combine,"_unit"),combine))%>%
    select(-tempo)%>%
    pivot_wider(names_from = combine,values_from=measurement_cat,names_sort=T)%>%
    mutate(year=substring(as.character(time_start),1,4))%>%
    rowwise()%>%
    mutate(period = ifelse(
      length(seq(time_start, time_end, by = "month"))==1, as.character(as.integer(substring(as.character(time_start),6,7))),
      ifelse(length(seq(time_start, time_end, by = "month"))==3,switch(substring(as.character(time_start),6,7),
                                                                       "01"="Q1",
                                                                       "04"="Q2",
                                                                       "07"="Q3",
                                                                       "10"="Q4",NA),NA)
    ))%>%
    ungroup()%>%
    select(-time_start,-time_end,-time)
  
  return(new_data)
}

