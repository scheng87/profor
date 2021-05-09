##Set up environment and constants
library(dplyr)
library(tidyr)
lut <- "questions_LU_cat.csv"
csv <- "Data_Final_PROFOR_7_2_18.csv"

##Define functions
#Move column to end or beginning of the dataframe
move.column <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  df.new <- switch(where,
                   first = data[c(tomove, temp)],
                   last = data[c(temp, tomove)])
  return(df.new)
}
#Subset the data based on categories of the Look-up table (LUT)
cat.subsetter <- function(dataset,expr){
  fields <- lut.cat %>% filter_(expr) %>% select(Field_name)
  list <- list(fields[[1]])[[1]]
  data.new <- dataset[,colnames(dataset) %in% list]
  return(data.new)
}
#Summarise the data per column (does not group by article)
summarizer <- function(dataset, fun.name){
  col.to.merge <- colnames(dataset)
  #Create a list of it
  dots <- lapply(col.to.merge, as.symbol)
  data.out <- dataset %>% summarise_each(funs(fun.name))
  data.out <- gather_(data.out,"field", as.character(substitute(fun.name)), dots)
  return(data.out)
}
#Summarise the data per journal article and creates a character list of all the values of the article
summarizer.list <- function(dataset){
  #new.field <- paste(field, "n", sep="_")
  data.l <- dataset %>% group_by(aid) %>% summarise_each(funs(toString))
  data.n <-  dataset %>% group_by(aid) %>% summarise_each(funs(n_distinct))
  data.out <- full_join(data.l, data.n, by="aid")
  return(data.out)
}
#List the unique values accross the whole table (ie, all the journal articles)
summarizer.list.all <- function(dataset, fields){
  summarise_each(dataset, funs(ul=toString(sort(unique(.))))) %>%
    gather_("field", "valuesList",fields) %>%
    select(field, valuesList)
}
##Summarise information groupiing by journal article with option to use two functions in a row, then gather the information to transpose the dataframe with fields used as rows
summarizer.groupby <- function(dataset, fun.name, fun.name2=NULL){
  #List the column names
  col.to.merge <- colnames(dataset)
  #Create a list of symbols from the column names
  dots <- lapply(col.to.merge[2:length(col.to.merge)], as.symbol)
  if (is.null(fun.name2)){
    data.out <- dataset  %>% summarise_each(funs(fun.name))
    data.out <- gather_(data.out,"field", as.character(substitute(fun.name)), dots) %>% select(-aid)
  } else {
    data.out <- dataset  %>% group_by(aid) %>% summarise_each(funs(fun.name)) %>% summarise_each(funs(fun.name2))
    data.out <- gather_(data.out,"field", as.character(substitute(fun.name2)), dots) %>% select(-aid)
  }
  return(data.out)
}
##Merge the multi-value binary coded fields into one column with repeated entries
combine.binfield <- function(datasubset,field, delete.option=1){
  #Get the binary fields
  #data.bin <- datasubset %>% select(contains(field))
  data.bin <- datasubset %>% select(starts_with(field))
  col.to.merge <- colnames(data.bin)
  #Create a list of it
  dots <- lapply(col.to.merge, as.symbol)
  #Keep only the binary columns
  data.bin <-  datasubset[,colnames(datasubset) %in% c("aid",col.to.merge)]
  #Merge the binary fields
  #data.binlong <- datasubset %>% gather_(field, "have", dots)
  data.binlong <- data.bin %>% gather_(field, "have", dots)
  #test to delete the rows with 0
  if (delete.option == 1) {
    data.binlong <- data.binlong %>% filter(have==1) %>% separate_(field, c("prefix",field), sep = "\\.")
  } else {
    data.binlong <- data.binlong %>% separate_(field, c("prefix",field), sep = "\\.")
    data.binlong[,field] <- ifelse(data.binlong$have == 0,NA, data.binlong[,field])
    data.binlong <- unique(data.binlong)
  }
  #Remove the temporary fields
  data.binlong$prefix <- NULL
  data.binlong$have <- NULL
  #transform aid to numeric
  data.binlong <- data.binlong %>% transform(aid = as.integer(aid))
  #Sort data using aid
  data.binlong <- arrange(data.binlong,aid)
  return(data.binlong)
}

##Merge the multi-value binary coded fields into one column with repeated entries using outcome id (OID)
combine.binfield2 <- function(datasubset,field, delete.option=1){
  #Get the binary fields
  #data.bin <- datasubset %>% select(contains(field))
  data.bin <- datasubset %>% select(starts_with(field))
  col.to.merge <- colnames(data.bin)
  #Create a list of it
  dots <- lapply(col.to.merge, as.symbol)
  #Keep only the binary columns
  data.bin <-  datasubset[,colnames(datasubset) %in% c("oid",col.to.merge)]
  #Merge the binary fields
  #data.binlong <- datasubset %>% gather_(field, "have", dots)
  data.binlong <- data.bin %>% gather_(field, "have", dots)
  #test to delete the rows with 0
  if (delete.option == 1) {
    data.binlong <- data.binlong %>% filter(have==1) %>% separate_(field, c("prefix",field), sep = "\\.")
  } else {
    data.binlong <- data.binlong %>% separate_(field, c("prefix",field), sep = "\\.")
    data.binlong[,field] <- ifelse(data.binlong$have == 0,NA, data.binlong[,field])
    data.binlong <- unique(data.binlong)
  }
  #Remove the temporary fields
  data.binlong$prefix <- NULL
  data.binlong$have <- NULL
  return(data.binlong)
}
##Assign outcome type to outcome subtype
assignOutGroup <- function(input,output){
  rows <- c(1:nrow(input))
  out_groups <- matrix(nrow=nrow(input),ncol=1)
  rownames(out_groups) <- rows
  colnames(out_groups) <- c("Out_type_assigned")
  for (i in rows){
    out <- as.vector(input$Out_subtype[i])
    if (out == "mon_dir" | out == "mon_wage" | out == "mon_val" | out == "phys_cons") {
      group <- "inc_con"
    } else 
      group <- "cap_asset"
    out_groups[i,"Out_type_assigned"] <- group
  }
  out_groups <- as.data.frame(out_groups)
  output <- bind_cols(input,out_groups)
  output <- filter(output,!is.na(out_groups))
  return(output)
}

##--------------------------------------------------------
##Begin processing data 
##--------------------------------------------------------

##Read in data and subset
data <- read.csv(csv, head=TRUE, sep=",", colClasses="character",encoding="utf-8")
data <- filter(data, aid != "")
data <- filter(data, aid != " ")
# Look-up table with the categories and attributes
lut.cat <- read.csv(lut, head=TRUE, sep=",", colClasses="character")
#list field names
field.names <- colnames(data)
data.biblio.raw <- cat.subsetter(data,quote(Category == "index" | Category == "A" | Category == "R"))
data.interv.raw <- cat.subsetter(data,quote(Category == "index" | Category == "I"))
data.study.raw <- cat.subsetter(data,quote(Category == "index" | Category == "S"))
data.outcome.raw <- cat.subsetter(data,quote(Category == "index" | Category == "O"))
data.pathways.raw <- cat.subsetter(data,quote(Category == "index" | Category == "P"))
data.biomes.study <- cat.subsetter(data,quote(Category == "index" | Category == "BS"))
##Merge multivalued fields
##Bibliographic information
data.biblio.affil <- combine.binfield(data.biblio.raw, "Affil_type")
#Summarise the data
summary.byaid <- summarizer.list(data.biblio.affil)
#Remove the binary fields from the raw data
data.biblio <- select(data.biblio.raw,-starts_with("Affil_type"))  %>%
  transform(aid = as.integer(aid))

#Join the table
data.biblio <- full_join(data.biblio, data.biblio.affil, by="aid")
data.biblio <- distinct(data.biblio)

#cleanup
rm(list=ls(pattern="data.biblio."))

##Intervention

#Intervention type
#Combine the binary fields
data.interv.Int_type <- combine.binfield(data.interv.raw, "Int_type")
#Summarise the data
summary.byaid <- summarizer.list(data.interv.Int_type) %>% right_join(summary.byaid,by="aid")

#Intervention - geographic scale
#Combine the binary fields
data.interv.Int_geo <- combine.binfield(data.interv.raw, "Int_geo")
#Summarise the data
summary.byaid <- summarizer.list(data.interv.Int_geo) %>% right_join(summary.byaid,by="aid")

#Implementation type
#Combine the binary fields
data.interv.Impl <- combine.binfield(data.interv.raw, "Impl_type")
#Summarise the data
summary.byaid <- summarizer.list(data.interv.Impl) %>% right_join(summary.byaid,by="aid")

#Merge the binary cases
data.interv.combined <- full_join(data.interv.Int_type, data.interv.Int_geo, by="aid") %>% distinct()
data.interv.combined <- full_join(data.interv.combined, data.interv.Impl, by="aid") %>% distinct()
data.interv.combined <- distinct(data.interv.combined)

#Remove the binary fields from the raw data
data.interv <- select(data.interv.raw,-starts_with("Int_type")) %>%
  select(-starts_with("Impl_type")) %>% select(-starts_with("Int_geo")) %>% 
  transform(aid = as.integer(aid))
#Join the table
data.interv <- full_join(data.interv, data.interv.combined, by="aid")
data.interv <- distinct(data.interv)
#cleanup
rm(list=ls(pattern="data.interv."))

##Study information

#Evaluation affiliation type
#Combine the binary fields
data.study.affil <- combine.binfield(data.study.raw, "Eval_affil_type")
#Summarise the data
summary.byaid <- summarizer.list(data.study.affil) %>% right_join(summary.byaid,by="aid")

#Study type
#Combine the binary fields
data.study.type <- combine.binfield(data.study.raw, "Study_type")
#Summarise the data
summary.byaid <- summarizer.list(data.study.type) %>% right_join(summary.byaid,by="aid")

#Comparison type
#Combine the binary fields
data.study.comps <- combine.binfield(data.study.raw, "Comps_type")
#Summarise the data
summary.byaid <- summarizer.list(data.study.type) %>% right_join(summary.byaid,by="aid")

#Merge the binary cases
data.study.combined <- full_join(data.study.type, data.study.affil, by="aid")
data.study.combined <- full_join(data.study.combined, data.study.comps, by="aid")
data.study.combined <- distinct(data.study.combined)

#Remove the binary fields from the raw data
data.study <- select(data.study.raw,-starts_with("Eval_affil_type")) %>% select(-starts_with("Study_type")) %>% select(-starts_with("Comps_type")) %>% transform(aid = as.integer(aid))
#Join the table
data.study <- full_join(data.study, data.study.combined, by="aid")
data.study <- distinct(data.study)

#cleanup
rm(list=ls(pattern="data.study."))

##Biomes
#Combine the binary fields
data.biomes <- combine.binfield(data.biomes.study, "Biome.")
#Summarise the data
summary.byaid <- summarizer.list(data.biomes) %>% right_join(summary.byaid,by="aid")
#cleanup
rm(list=ls(pattern="data.biomes.study"))

##Outcomes
data.outcome.raw <- distinct(data.outcome.raw)
#Assign oid
m <- select(data.outcome.raw, aid) %>% distinct()
aids <- m$aid
data.outcome.test <- as.data.frame(matrix(ncol=22,nrow=1))
colnames(data.outcome.test) <- colnames(data.outcome.raw)
data.outcome.test$aid <- as.integer(data.outcome.test$aid)
data.outcome.test$oid <- c("")

for (x in aids){
  sub <- filter(data.outcome.raw, aid == x)
  rows <- c(1:nrow(sub))
  sub$oid <- c("") 
  for (i in rows){
    n <- paste(as.character(sub[i,1]),".",i,sep="")
    sub[i,23] <- n 
  }
  sub$aid <- as.integer(sub$aid)
  data.outcome.test <- bind_rows(data.outcome.test,sub)
}
data.outcome.test <- slice(data.outcome.test, -1)

#Sub-Type
#Combine the binary fields
data.outcome.subtype <- combine.binfield2(data.outcome.test, "Out_subtype")

#Assign outcome_types
data.outcome.type <- assignOutGroup(data.outcome.subtype)

#Summarise the data
summary.byaid <- summarizer.list(data.outcome.type) %>% right_join(summary.byaid,by="aid")

#Remove the binary fields from the raw data
data.outcome <- select(data.outcome.test,-starts_with("Out_subtype")) %>% transform(aid = as.integer(aid))
#Join the table
data.outcome <- full_join(data.outcome, data.outcome.type, by="oid")
data.outcome <- distinct(data.outcome)
#cleanup
rm(list=ls(pattern="data.outcome."))

##Pathways
#PRIME
#Combine the binary fields
data.pathways.prime <- combine.binfield(data.pathways.raw, "PRIME")
#Summarise the data
summary.byaid <- summarizer.list(data.pathways.prime) %>% right_join(summary.byaid,by="aid")

#Combine the binary fields
data.pathways.model <- combine.binfield(data.pathways.raw, "Model_use")
#Summarise the data
summary.byaid <- summarizer.list(data.pathways.model) %>% right_join(summary.byaid,by="aid")

#Merge the binary cases
data.pathways.combined <- full_join(data.pathways.prime, data.pathways.model, by="aid")
data.pathways.combined <- distinct(data.pathways.combined)

#Remove the binary fields from the raw data
data.pathways <- select(data.pathways.raw,-starts_with("PRIME")) %>% select(-starts_with("Model_use")) %>% transform(aid = as.integer(aid))
#Join the table
data.pathways <- full_join(data.pathways, data.pathways.combined, by="aid")
data.pathways <- distinct(data.pathways)
data.pathways$aid <- as.integer(data.pathways$aid)
rm(list=ls(pattern="data.pathways."))

data.biblio <- data.biblio[!apply(is.na(data.biblio) | data.biblio == "", 1, all),]
data.biomes <- data.biomes[!apply(is.na(data.biomes) | data.biomes == "", 1, all),]
data.interv <- data.interv[!apply(is.na(data.interv) | data.interv == "", 1, all),]
data.outcome <- data.outcome[!apply(is.na(data.outcome) | data.outcome == "", 1, all),]
data.study <- data.study[!apply(is.na(data.study) | data.study == "", 1, all),]
data.pathways <- data.pathways[!apply(is.na(data.pathways) | data.pathways == "", 1, all),]

##Write final R data and package
save(data.biblio,data.interv,data.study,data.biomes,data.outcome,data.pathways, file = "PROFOR_Evidence_Map_7_5_2018.RData")
