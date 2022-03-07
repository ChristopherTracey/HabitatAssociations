library(tidyverse)
library(arcgisbinding)
arc.check_product()


flag_datatype <- "areaXpoly" # "point" "ranpt" "area" "areaXpoly"

# get SGCN data
lu_sgcn <- arc.open("D:/COA_Tools/_data/output/_update2021q4/SGCN.gdb/allSGCNuse")
lu_sgcn <- arc.select(lu_sgcn, fields = c("SNAME", "ELSeason"))
lu_sgcn <- unique(lu_sgcn)


# load SGCN data in
if(flag_datatype=="point"){
  sgcn <- arc.open("D:/HabitatMapAssociations/HabitatMapAssociations.gdb/allSGCNuse_Point1")
  sgcn <- arc.select(sgcn)
  sgcn <- sgcn[which(!is.na(sgcn$Ecological_System___NEW)),]
  sgcn1 <- sgcn[c("ELSeason", "Ecological_System___NEW")]
  sgcn1$pres <- 1
  colnames(sgcn1)[2] <- "Habitat"
  sgcn1 <- sgcn1[c("Habitat","ELSeason","pres" )]
  sgcn_wide <- sgcn1 %>% pivot_wider(names_from = ELSeason, values_from=pres, values_fn=sum)
} else if(flag_datatype=="area"){
  sgcn <- arc.open("D:/HabitatMapAssociations/HabitatMapAssociations.gdb/Tabulat_allSGCN1")
  sgcn <- arc.select(sgcn)
  sgcn <- sgcn[which(!is.na(sgcn$Ecological_System___NEW)),]
  sgcn1 <- sgcn[c("ELSeason", "Ecological_System___NEW", "Count")] # can use area if we wanted, didn't to 
  colnames(sgcn1)[2] <- "Habitat"
  sgcn1 <- sgcn1[c("Habitat","ELSeason","Count" )]
  # sgcn1 <- sgcn1[which(sgcn1$Habitat!="Developed"),]  ### option for removing developed...
  sgcn_wide <- sgcn1 %>% pivot_wider(names_from = ELSeason, values_from=Count, values_fn=sum)
} else if(flag_datatype=="ranpt"){
  sgcn <- arc.open("D:/HabitatMapAssociations/HabitatMapAssociations.gdb/allSGCNuse_RandomPts_20")
  sgcn <- arc.select(sgcn)
  sgcn <- sgcn[which(!is.na(sgcn$Ecological_System___NEW)),]
  sgcn1 <- sgcn[c("ELSeason", "Ecological_System___NEW")]
  sgcn1$pres <- 1
  colnames(sgcn1)[2] <- "Habitat"
  sgcn1 <- sgcn1[c("Habitat","ELSeason","pres" )]
  sgcn_wide <- sgcn1 %>% pivot_wider(names_from = ELSeason, values_from=pres, values_fn=sum)  
} else if(flag_datatype=="areaXpoly"){
  sgcn <- arc.open("D:/HabitatMapAssociations/HabitatMapAssociations.gdb/Tabulat_allSGCN1_oid")
  sgcn <- arc.select(sgcn)
  sgcn <- sgcn[which(!is.na(sgcn$Ecological_System___NEW)),]
  
  sgcn1 <- sgcn[c("OBJECTID_1","ELSeason", "Ecological_System___NEW", "Count")] # can use area if we wanted, didn't see the need as we just need to multiply by 100 
  colnames(sgcn1)[3] <- "Habitat"
  sgcn1 <- sgcn1[c("OBJECTID_1","Habitat","ELSeason","Count" )]
  # sgcn1 <- sgcn1[which(sgcn1$Habitat!="Developed"),]  ### option for removing developed...
  sgcn_wide <- sgcn1 %>% pivot_wider(names_from=ELSeason, values_from=Count, values_fn=sum)  
} else {
  cat("not a valid entry")
} 



# further processing on the wide version
if(flag_datatype!="areaXpoly"){
  sgcn_wide[is.na(sgcn_wide)] <- 0 # convert NA to 0's
  sgcn_wide$hab <- 1:nrow(sgcn_wide)
  habitats <- sgcn_wide[c("Habitat","hab")]
  sgcn_wide$Habitat <- NULL
  sgcn_wide$hab <- NULL
} else if(flag_datatype=="areaXpoly"){
  sgcn_wide[is.na(sgcn_wide)] <- 0 # convert NA to 0's
  sgcn_wide <- sgcn_wide %>% group_by(Habitat) %>% mutate(id=cur_group_id())
    sgcn_wide$hab <- sgcn_wide$id
  habitats <- sgcn_wide[c("Habitat","hab")]
  sgcn_wide$Habitat <- NULL
  sgcn_wide$hab <- NULL
}

# run the indicator analysis
library(labdsv)

res <- 
  indval(x=sgcn_wide, clustering=habitats$hab, type="short", numitr=500)

res_indval <- res$indval

if(flag_datatype!="areaXpoly"){
  res_indval <- res_indval %>% rename_at(vars(names(res_indval)), ~habitats$Habitat)
} else if(flag_datatype=="areaXpoly"){
  habitats_unq <- unique(habitats)
  res_indval <- res_indval %>% rename_at(vars(names(res_indval)), ~habitats_unq$Habitat)  
}  

res_indval$ELSeason <- row.names(res_indval)
res_indval1 <- merge(res_indval, lu_sgcn, by="ELSeason")

write.csv(res_indval1, paste("res_indval_", flag_datatype,"_",gsub(" ","_",gsub(c("-|:"),"",as.character(Sys.time()))), ".csv", sep="")   )



