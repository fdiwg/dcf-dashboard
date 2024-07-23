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

