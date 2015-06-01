avgAmtByJob = function(job, wildcards = c("gift", "camera", "ipad", "ipod", "tv")){
  
  if(is.null(people) || is.null(purchases))
    return(NULL)
  
  ids = subset(people, people$DEPARTMENT == job)$EMPL_ID #TODO: check department is the field I want
  #TODO: ensure I care about case sensitivity
  
  amounts = subset(purchases, purchases$"EMPL ID" %in% ids, select = c('EMPL ID', 'TRAN AMT', 'COST CODE', 'EXPENSE DESC'))
  amounts = droplevels(amounts)

  m = mean(as.numeric(as.character(amounts$'TRAN AMT')), na.rm = T)
  
  f = mean(tapply(amounts$'EMPL ID', amounts$'EMPL ID', length), na.rm = T)
  
  r = tapply(amounts$'EMPL ID', list(amounts$'EMPL ID', amounts$'COST CODE'), length)
  r[is.na(r)] = 0
  r = colMeans(r) #note that if it's not listed in R that means no person in the job has purchased from this relater type
  
  w = mean(as.numeric(sapply(amounts$'EXPENSE DESC', function(x) grepl(paste(wildcards, collapse = "|"), x))))
  
  list(m, f, r, w)
}

avgAmtByID = function(id, wildcards = c("gift", "camera", "ipad", "ipod", "tv")){
    
  if(is.null(people) || is.null(purchases))
    return(NULL)
  
  amounts = subset(purchases, purchases$"EMPL ID" == id, select = c('EMPL ID', 'TRAN AMT', 'COST CODE', 'EXPENSE DESC'))
  amounts = droplevels(amounts)
  
  m = mean(as.numeric(as.character(amounts$'TRAN AMT')), na.rm = T)
  
  f = nrow(amounts)
  
  r = tapply(amounts$'EMPL ID', list(amounts$'EMPL ID', amounts$'COST CODE'), length)
  r[is.na(r)] = 0
  r = colMeans(r) #note that if it's not listed in R that means no person in the job has purchased from this relater type
  
  w = mean(as.numeric(sapply(amounts$'EXPENSE DESC', function(x) grepl(paste(wildcards, collapse = "|"), x))))
  
  list(m, f, r, w)
  
}