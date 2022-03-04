library(tidyverse)
library(arcgisbinding)
arc.check_product()


flag_datatype <- "area" # "point"

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
  sgcn1 <- sgcn1[which(sgcn1$Habitat!="Developed"),]
  sgcn_wide <- sgcn1 %>% pivot_wider(names_from = ELSeason, values_from=Count, values_fn=sum)
} else {
  cat("not a valid entry")
}  

# further processing on the wide version
sgcn_wide[is.na(sgcn_wide)] <- 0 # convert NA to 0's
sgcn_wide$hab <- 1:nrow(sgcn_wide)
habitats <- sgcn_wide[c("Habitat","hab")]
sgcn_wide$Habitat <- NULL
sgcn_wide$hab <- NULL


# run the indicator analysis
library(labdsv)
sgcn_widetmp <- sgcn_wide[1:340]

res <- 
  indval(x=sgcn_widetmp, clustering=habitats$hab, type="short", numitr=500)

res_indval <- res$indval

res_indval <- res_indval %>% rename_at(vars(names(res_indval)), ~habitats$Habitat)
res_indval$ELSeason <- row.names(res_indval)

res_indval1 <- merge(res_indval, lu_sgcn, by="ELSeason")

write.csv(res_indval1, "res_indval_area.csv")

# 
# sgcnkm = kmeans(sgcn_wide, centers=44)
# groupskm = sgcnkm$cluster
# 
# a <- multipatt(sgcn_wide[,2:8], sgcn_wide$Habitat, control = how(nperm=999))
# 
# 
# #####
# #sgcn$spseason <- paste(sgcn$SNAME, " (",sgcn$SeasonCode,")", sep="")
# #sgcn$SNAME <- NULL
# #sgcn$SeasonCode <- NULL
# 
# colnames(sgcn)[1] <- "Habitat"
# 
# library(tidyverse)
# 
# sgcn$FREQUENCY <- as.integer(sgcn$FREQUENCY)
# 
# sgcn_wide <- sgcn %>% pivot_wider(names_from = spseason, values_from = FREQUENCY)
# sgcn_wide <- sgcn_wide %>% remove_rownames %>% column_to_rownames(var="Habitat")
# sgcn_wide[is.na(sgcn_wide)] <- 0
# sgcn_wide <- as.dataframe.matrix(sgcn_wide)
# 
# sgcnkm = kmeans(sgcn_wide, centers=44)
# groupskm = sgcnkm$cluster
# 
# 
# a <- multipatt(sgcn_wide, groupskm, control = how(nperm=999))
# 
