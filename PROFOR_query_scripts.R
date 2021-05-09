library(tidyr)
library(dplyr)
library(gplots)
library(RColorBrewer)

map_data_final <- readRDS("map_data_final_7_5_18.rds")
load("PROFOR_Evidence_Map_7_5_2018.RData")

###Define variables
out_type <- c("mon_dir","mon_wage","mon_val","phys_cons","fin","nat_for","nat_land","phys_cap","hum_cap","health","soc_cap")
int_type <- c("for_mgmt","agrofor","hab_mgmt","gov","emp","prod_cap","hum_cap","soc_cap","liv_alt","market","mark_acc","eco_ser","instit","non_mon")

int_labels =c("Forest mgmt","Agroforestry","Habitat mgmt","Governance","Indiv. empowerment","Produced capital","Human capital","Social capital","Linked enterprises/livelihood alt.","Market forces","Market access","Managing/enhancing eco. sys. serv.","Institutions & markets","Non-monetary benefits")
out_labels = c("Mon. income (direct sale)","Mon. income (wage labor)","Mon. income (value added)","Phys. income (consumption)","Fin. cap. (credit, savings,debt)","Nat. cap. (forest assets)","Nat. cap (land assets)","Phys. cap. (material assets)","Hum. cap. (knowledge, skills)","Health","Soc. cap.")

study_types <- c("exp","quas_exp","non_exp","imp_ev","non_sys_rev","sys_rev","NA")
study_labels <- c("Experimental","Quasi-experimental","Non-experimental","Impact evaluation","Non-systematic review","Systematic review","Unknown")
comp_types <- c("ba","between","cont","pa","punc","spat","other","none","NA")
comp_labels <- c("Before/after","Between groups","Continuous time series","Presence/absence","Punctuated time series","Spatial","Other","None","Unknown")

biome_labels <- c("Deserts/xeric scrublands","Mangroves","Montane forests, woodlands, shrublands","Temperate broadleaf and mixed","Temperate coniferous","Tropical/subtropical coniferous","Tropical/subtropical dry broadleaf","Tropical/subtropical moist broadleaf")

forest_labels <- c("Other non-forest env. w/ natural veg.","Other non-forest env. w/ planted trees, trees on farms, or tree farms","Managed plantation","Old-growth","Secondary or regenerating")
data <- filter(map_data_final, Study_type != "sys_rev") %>% distinct()
data <- map_data_final

##Set plotting defaults
mar.default <- c(5,4,4,2) + 0.1

###Only run if want to disaggregate summary stats by PRIME category
# P <- filter(map_data_final, Int_type == "for_mgmt" | Int_type == "agrofor" | Int_type == "hab_mgmt") %>% distinct()
# R <- filter(map_data_final, Int_type == "gov" | Int_type == "emp") %>% distinct()
# I <- filter(map_data_final, Int_type == "prod_cap" | Int_type == "hum_cap" | Int_type == "soc_cap") %>% distinct()
# M <- filter(map_data_final, Int_type == "liv_alt" | Int_type == "market" | Int_type == "mark_acc") %>% distinct()
# E <- filter(map_data_final, Int_type == "eco_ser" | Int_type == "instit" | Int_type == "non_mon") %>% distinct()
# 
# data <- E
# int_type <- c("eco_ser","instit","non_mon")
# int_labels <- c("Managing/enhancing eco. sys. serv.","Institutions & markets","Non-monetary benefits")
# 
# data <- M
# int_type <- c("liv_alt","market","mark_acc")
# int_labels <- c("Linked enterprises/livelihood alt.","Market forces","Market access")
# 
# data <- I
# int_type <- c("prod_cap","hum_cap","soc_cap")
# int_labels <- c("Produced capital","Human capital","Social capital")
# 
# data <- R
# int_type <- c("gov","emp")
# int_labels <- c("Governance","Indiv. empowerment")
# 
# data <- P
# int_type <- c("for_mgmt","agrofor","hab_mgmt")
# int_labels <- c("Forest mgmt","Agroforestry","Habitat mgmt")
# 
# data <- filter(map_data_final, Study_type == "sys_rev") %>% distinct() %>% filter(aid == 238 | aid == 237)

###Count linkages between intervention and outcome types and plot in heatmap
io_counts = matrix(nrow=11, ncol=14)
rownames(io_counts) <- out_type
colnames(io_counts) <-int_type

for (i in int_type){
  for (j in out_type){
    subset <- filter(data, Out_subtype == j, Int_type == i)
    io_counts[j,i] <- n_distinct(subset$aid)
  }
}

rownames(io_counts) <- out_labels
colnames(io_counts) <- int_labels

palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=50)
                                    
pdf(file="Interventions_Outcomes_EGM_Heatmap_7_18.pdf", width=11, height=8.5)
par(mar = mar.default + c(0, 0, 10, 0)) 
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=io_counts, notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

###Outcomes per article
io_counts = matrix(nrow=242, ncol=11)
rownames(io_counts) <- c(1:242)
colnames(io_counts) <-out_type

for (i in c(1:242)){
  for (j in out_type){
    subset <- filter(data, Out_subtype == j, aid == i)
    io_counts[i,j] <- n_distinct(subset$aid)
  }
}

io_counts <- as.data.frame(io_counts)
io_counts <- arrange(io_counts,desc(mon_dir))
io_counts <- arrange(io_counts,desc(mon_wage))
io_counts <- arrange(io_counts,desc(mon_val))
io_counts <- arrange(io_counts,desc(phys_cons))
io_counts <- arrange(io_counts,soc_cap)
io_counts <- arrange(io_counts,phys_cap)

rownames(io_counts) <- c(1:242)
colnames(io_counts) <- out_labels

io_counts <- as.matrix(io_counts)
palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=50)

pdf(file="Outcomes_per_article.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

only_mon <- filter(io_counts, mon_dir == 1 | mon_wage == 1 | mon_val == 1) %>% distinct()
only_mon <- filter(only_mon, phys_cons == 0 & fin == 0 & nat_for == 0 & nat_land == 0 & phys_cap == 0 & hum_cap == 0 & health == 0 & soc_cap == 0) %>% distinct()
nrow(only_mon)
#Plot distribution to PDF
library(ggplot2)
library(RColorBrewer)

###Distribution by geography
##Plot countries
#load in full country list
country <- read.csv("allcountries.csv", head=TRUE, sep=",")
names(country) <- c("Study_country", "Region", "Code", "Subregion")
regions <- country
regions <- arrange(regions,Region)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(regions), ncol=2)
rownames(country_count) <- regions$Study_country
colnames(country_count) <- c("Study_country", "counts")

#Calculate in for loop and write to blank matrix
for (c in regions$Study_country){
  subset <- filter(data, Study_country.x == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries_only <- inner_join(country_count,regions,by="Study_country")
countries_only <- filter(countries_only, Code != "")

countries_only$counts <- as.numeric(countries_only$counts)
countries_only <- as.data.frame(countries_only)
countries_zero <- filter(countries_only, counts == 0)

pdf(file="PROFOR_Country_Map_dark.pdf", width=16, height=8.5)
ggplot() + geom_map(data=countries_only, aes(map_id=Code, fill=counts),map=map) + geom_map(data=countries_zero, aes(map_id=Code),fill="#f0f0f0",map=map) + expand_limits(x=map$long,y=map$lat) + theme(panel.background = element_rect(fill = "black", colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_fill_gradient2(low="#d9f0a3",mid="#41ab5d",high="#004529",midpoint=(max(countries_only$counts)/2),limits=c(0,max(countries_only$counts)))
dev.off()

###Summary statistics by study design
design_summary <- matrix(nrow=7,ncol=1)
rownames(design_summary) <- study_types
colnames(design_summary) <- c("counts")
for (r in study_types){
  sub <- filter(data,Study_type == r)
  design_summary[r,1] <- n_distinct(sub$aid)
} 
design_summary <- as.data.frame(design_summary)

pdf(file="Study_type_summary_7_18.pdf", width=20, height=8.5)
par(mar = mar.default + c(0, 6, 0, 0)) 
barplot(design_summary$counts,horiz=TRUE,xlim=c(0,max(design_summary$counts)+5),names.arg=study_labels,las=1,xlab="Number of studies",cex.axis=1.5)
box()
dev.off()

#Summary statistics by comparator
comp_summary <- matrix(nrow=9,ncol=1)
rownames(comp_summary) <- comp_types
colnames(comp_summary) <- c("counts")
for (r in comp_types){
  sub <- filter(data,Comps_type == r)
  comp_summary[r,1] <- n_distinct(sub$aid)
} 
comp_summary <- as.data.frame(comp_summary)

pdf(file="Comp_summary_7_18.pdf", width=20, height=8.5)
par(mar = mar.default + c(0, 6, 0, 0)) 
barplot(comp_summary$counts,horiz=TRUE,xlim=c(0,120),names.arg=comp_labels,las=1,xlab="Number of studies",cex.axis=1.5)
box()
dev.off()

###Summary statistic by outcome type
out_summary <- matrix(nrow=length(out_type),ncol=1)
rownames(out_summary) <- out_type
colnames(out_summary) <- c("counts")
for (r in out_type){
  sub <- filter(data,Out_subtype == r)
  out_summary[r,1] <- n_distinct(sub$aid)
} 
out_summary <- as.data.frame(out_summary)

pdf(file="Out_type_summary.pdf", width=20, height=8.5)
barplot(out_summary$counts,horiz=TRUE,xlim=c(0,max(out_summary$counts)+5),names.arg=out_labels,las=1,xlab="Number of studies",cex.axis=1.5)
box()
dev.off()

###Summary statistics by intervention type
int_summary <- matrix(nrow=length(int_type),ncol=1)
rownames(int_summary) <- int_type
colnames(int_summary) <- c("counts")
for (r in int_type){
  sub <- filter(data,Int_type == r)
  int_summary[r,1] <- n_distinct(sub$aid)
} 
int_summary <- as.data.frame(int_summary)

pdf(file="Int_type_summary.pdf", width=20, height=8.5)
barplot(int_summary$counts,horiz=TRUE,xlim=c(0,max(int_summary$counts)+5),names.arg=int_labels,las=1,xlab="Number of studies",cex.axis=1.5)
box()
dev.off()

###Summary statistics by region
reg_summary <- matrix(nrow=5,ncol=1)
rownames(reg_summary) <- c("Asia","Africa","Europe","Latin America","Oceania")
colnames(reg_summary) <- c("counts")
for (r in reg){
  sub <- filter(countries_only,Region == r)
  sum <- sum(sub$counts)
  reg_summary[r,1] <- sum
} 
reg_summary <- as.data.frame(reg_summary)

pdf(file="Country_distribution_region.pdf", width=20, height=8.5)
barplot(reg_summary$counts,horiz=TRUE,xlim=c(0,max(reg_summary$counts)+5),names.arg=c("Asia","Africa","Europe","Latin America","Oceania"),las=1,xlab="Number of studies by region",cex.axis=1.5)
box()
dev.off()

###Intervention-outcome heatmap by study type
design_out <- select(data,aid,Study_type,Out_subtype) %>% distinct() %>% count(Study_type,Out_subtype)
design_out <- filter(design_out, Out_subtype != "NA")
design_out <- arrange(design_out,Out_subtype)
out <- select(data,aid,Out_subtype) %>% distinct() %>% count(Out_subtype)
int <- select(data,aid,Int_type) %>% distinct() %>% count(Int_type)

labels <- c(fin = "Financial capital",health = "Health", hum_cap = "Human capital", mon_dir = "Monetary income (direct sale)", mon_val = "Monetary income (value addition", mon_wage = "Monetary income (wage labor)",nat_for = "Natural capital (access to forest resources)",nat_land = "Natural capital (access to land resources)", phys_cap = "Physical capital",phys_cons = "Physical consumption", soc_cap = "Social capital") 
pdf(file = "Study_type_by_outcome.pdf",width=11, height=8.5)
ggplot(design_out, aes(x=Study_type,y=n)) + geom_bar(stat="identity") + facet_wrap(~ Out_subtype, ncol=3, labeller=labeller(Out_type = labels))
dev.off()

Disagg <- select(data,aid,Disagg.gender,Disagg.soc_eco,Disagg.race) %>% distinct() %>% count(Disagg.gender)

Sys_rev <- filter(data,Study_type == "sys_rev") %>% distinct()

###Summary statistics by year
y <- c(1994:2016)
year_summary <- matrix(nrow=23,ncol=1)
rownames(year_summary) <- y
colnames(year_summary) <- c("counts")
for (i in y){
  sub <- filter(data,Pub_year == i)
  year_summary[as.character(i),1] <- n_distinct(sub$aid)
}
year_summary <- as.data.frame(year_summary)

pdf(file="Publications_by_year.pdf", width=20, height=8.5)
barplot(year_summary$counts,ylim=c(0,35),names.arg=c(1994:2016),las=1,xlab="Number of studies published each year",cex.axis=1.5)
box()
dev.off()

##Comparators by study type
data <- select(data,aid,Study_type,Comps_type) %>% distinct()
data$Study_type <- as.character(data$Study_type)
data$Comps_type <- as.character(data$Comps_type)
data$Study_type[is.na(data$Study_type)] <- "NA"
data$Comps_type[is.na(data$Comps_type)] <- "NA"

io_counts = matrix(nrow=7, ncol=9)
rownames(io_counts) <- study_types
colnames(io_counts) <-comp_types

for (i in comp_types){
  for (j in study_types){
    subset <- filter(data, Study_type == j, Comps_type == i)
    io_counts[j,i] <- n_distinct(subset$aid)
  }
}

rownames(io_counts) <- study_labels
colnames(io_counts) <- comp_labels

palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=50)

pdf(file="Study_type_by_Comp_type.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=io_counts, notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Indicators
ind <- select(data,aid,Out_subtype,Indicators,Outcome.data_type) %>% distinct()
ind <- arrange(ind,Out_subtype,Indicators,Outcome.data_type)
write.csv(ind,"Indicators_PROFOR.csv")

##Outcomes per region/subregion

countries <- count(data,subregion)
countries <- countries$subregion
io_counts <- matrix(nrow=14,ncol=11)
colnames(io_counts) <-out_type
rownames(io_counts) <- countries

#Calculate in for loop and write to blank matrix
for (c in countries){
  for(j in out_type){
    subset <- filter(data, Out_subtype == j, subregion == c)
    io_counts[c,j] <- n_distinct(subset$aid)
  }
}

io_counts <- as.data.frame(io_counts)
io_counts <- arrange(io_counts,desc(mon_dir))
io_counts <- arrange(io_counts,desc(mon_wage))
io_counts <- arrange(io_counts,desc(mon_val))
io_counts <- arrange(io_counts,desc(phys_cons))
io_counts <- arrange(io_counts,soc_cap)
io_counts <- arrange(io_counts,phys_cap)

colnames(io_counts) <- out_labels
rownames(io_counts) <- countries

io_counts <- as.matrix(io_counts)
palette_final <- colorRampPalette(c("white", "green","blue","violet")) (n=50)

pdf(file="Outcomes_per_country.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=io_counts,notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Interventions per article <- PRIME
io_counts = matrix(nrow=242, ncol=14)
rownames(io_counts) <- c(1:242)
colnames(io_counts) <-int_type

for (i in c(1:242)){
  for (j in int_type){
    subset <- filter(data, Int_type == j, aid == i)
    io_counts[i,j] <- n_distinct(subset$aid)
  }
}

io_counts <- as.data.frame(io_counts)
P <- select(io_counts, for_mgmt, agrofor, hab_mgmt)
R <- select(io_counts, gov, emp)
I <- select(io_counts, prod_cap, hum_cap, soc_cap)
M <- select(io_counts,liv_alt, market, mark_acc)
E <- select(io_counts, eco_ser, instit, non_mon)
P_sum <- as.data.frame(rowSums(P))
R_sum <- as.data.frame(rowSums(R))
I_sum <- as.data.frame(rowSums(I))
M_sum <- as.data.frame(rowSums(M))
E_sum <- as.data.frame(rowSums(E))
sums <- bind_cols(P_sum,R_sum,I_sum,M_sum,E_sum)
colnames(sums) <- c("P","R","I","M","E")
sums[sums > 0] <- 1
sums$total <- rowSums(sums)
sums$aid <- rownames(sums)

counts <- count(sums,total)
counts <- mutate(counts, percent = n/241 *100)
io_counts <- as.matrix(io_counts)
palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=50)

pdf(file="Outcomes_per_article.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

#Heterogeneity
disagg <- select(map_data_final,aid,Disagg.gender,Disagg.race,Disagg.soc_eco) %>% distinct()
disagg$total <-c("")
x = ifelse(disagg$Disagg.gender == "Yes" | disagg$Disagg.race == "Yes" | disagg$Disagg.soc_eco == "Yes",1,0)
disagg = cbind(x,disagg)
count(disagg,x)
