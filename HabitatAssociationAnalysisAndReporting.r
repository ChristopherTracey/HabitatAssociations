library(tidyverse)
library(arcgisbinding)
arc.check_product()
library(labdsv)
library(ggplot2)
library(grid)
library(scales)
require(knitr)
require(tinytex)
require(english)

# function to generate the pdf
#knit2pdf(here::here("scripts","template_Formatted_NHA_PDF.rnw"), output=paste(pdf_filename, ".tex", sep=""))
makePDF <- function(rnw_template, pdf_filename) {
  knit(here::here(rnw_template), output=paste(pdf_filename, ".tex",sep=""))
  call <- paste0("xelatex -interaction=nonstopmode ", pdf_filename , ".tex")
  system(call)
  system(paste0("biber ",pdf_filename))
  system(call) # 2nd run to apply citation numbers
}

# function to delete .txt, .log etc if pdf is created successfully.
deletepdfjunk <- function(pdf_filename){
  fn_ext <- c(".aux",".out",".run.xml",".bcf",".blg",".tex",".log",".bbl",".toc") #
  if (file.exists(paste(pdf_filename, ".pdf",sep=""))){
    for(i in 1:NROW(fn_ext)){
      fn <- paste(pdf_filename, fn_ext[i],sep="")
      if (file.exists(fn)){
        file.remove(fn)
      }
    }
  }
}

options(scipen = 999)


###########################################################
flag_datatype <- "ranpt" # "point" "ranpt" "area" "areaXpoly"

# get SGCN data
lu_sgcn <- read.csv("lu_SGCN_update2021q4.csv", stringsAsFactors = FALSE)

# load SGCN data in
sgcn <- arc.open("D:/HabitatMapAssociations/HabitatMapAssociations.gdb/allSGCNuse_RandomPts_20")
sgcn <- arc.select(sgcn)
sgcn <- sgcn[which(!is.na(sgcn$Ecological_System___NEW)),]
sgcn1 <- sgcn[c("CID","ELSeason", "Ecological_System___NEW")] # can use area if we wanted, didn't see the need as we just need to multiply by 100 
sgcn1$pres <- 1
colnames(sgcn1)[3] <- "Habitat"
sgcn1 <- sgcn1[c("CID","Habitat","ELSeason","pres" )]
sgcn1 <- merge(sgcn1, lu_sgcn[c("ELSeason", "TaxaDisplay")], by="ELSeason")
sgcn1 <- sgcn1[which(sgcn1$TaxaDisplay!="Invertebrate - Cave Invertebrates"&sgcn1$TaxaDisplay!="Invertebrate - Mussels"&sgcn1$TaxaDisplay!="Fish"),]
sgcn1$TaxaDisplay <- NULL
sgcn1 <- sgcn1[order(sgcn1$CID),]

# make a loop to do the indicator value analysis in

elseason_vector <- unique(sgcn1$ELSeason)

indval_metrics <- data.frame(
                    ELSeason=character(),
                    pvalue=double(),
                    grpcnt=integer(),
                    stringsAsFactors=FALSE
                  )

for(y in 1:40){ #length(elseason_vector)
  #make a temp dataframe to do the indicator analysis on
  sgcn_sub <- sgcn1[which(sgcn1$ELSeason==elseason_vector[y]),]
  # convert to wide format
  sgcn_wide <- sgcn_sub %>% pivot_wider(names_from = ELSeason, values_from=pres, values_fn=sum) 
  sgcn_wide[is.na(sgcn_wide)] <- 0 # convert NA to 0's
  sgcn_wide <- sgcn_wide[order(sgcn_wide$CID),]
  
  grps <- sgcn_wide$CID
  
  # group by habitats and make habitat list
  sgcn_wide <- sgcn_wide %>% group_by(Habitat) %>% mutate(hab=cur_group_id())
  habitats <- sgcn_wide[c("Habitat","hab")]
  habitats_unq <- unique(habitats)
  habitats_unq <- habitats_unq[order(habitats_unq$hab),]
  sgcn_wide$Habitat <- NULL
  sgcn_wide$hab <- NULL
  
  sgcn_wide <- sgcn_wide[, -which(names(sgcn_wide) %in% c("id","CID"))]
  
  # indicator value analysis
  res <- indval(x=sgcn_wide, clustering=habitats$hab, type="short", numitr=100)
  res_indval <- res$indval
  res_indval <- res_indval %>% rename_at(vars(names(res_indval)), ~habitats_unq$Habitat)

  res_indval$ELSeason <- row.names(res_indval)
  res_indval1 <- merge(res_indval, lu_sgcn[c("ELSeason","SNAME","SCOMNAME","TaxaDisplay")], by="ELSeason")
  
  cat("------------------------------\n")
  cat("working on ", unique(res_indval1$SCOMNAME),"\n")
  cat("the p-value is: ", res$pval,"\n")
  
  #add pvalue to list
  indval_metrics <- rbind(indval_metrics, data.frame(ELSeason=elseason_vector[y], pvalue=res$pval, grpcnt=length(unique(grps)))) 
  
  res_indval1 <- res_indval1 %>% relocate(any_of(c("SNAME", "SCOMNAME", "TaxaDisplay")), .after=ELSeason)
  
  res_indval_long <- res_indval1 %>% pivot_longer(!c("ELSeason","SNAME","SCOMNAME","TaxaDisplay"), names_to = "Habitat", values_to = "IndVal")
  res_indval_long <- res_indval_long[which(res_indval_long$IndVal>0),]
  res_indval_long <- res_indval_long[order(res_indval_long$ELSeason, res_indval_long$IndVal,decreasing=TRUE),]
  
  # make a bar chart for each species
  res_indval_long <- res_indval_long[order(res_indval_long$IndVal, decreasing=TRUE),]
  a <- ggplot(res_indval_long, aes(reorder(x=Habitat, IndVal), y=IndVal)) +
    geom_col(width = 0.7) +
    ylim(0, 1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width=50)) +
    coord_flip() +
    #ggtitle(unique(res_indval_long$SCOMNAME)) +
    theme(
      #plot.title = element_blank(),
      axis.title.y = element_blank()
    )
  ggsave(filename = here::here("_data","output",paste("habgraph_", unique(res_indval_long$ELSeason),".png", sep="")), width=6, height=nrow(res_indval_long)*.27, units="in")

}  









# ###
# res$indcls
# 
# 
# res$pval
# 
# 
# 
# res_indval$ELSeason <- row.names(res_indval)
# res_indval1 <- merge(res_indval, lu_sgcn, by="ELSeason")
# 
# res_indval1 <- res_indval1 %>% relocate(SNAME)
# 
# write.csv(res_indval1, paste("res_indval_", flag_datatype,"_",gsub(" ","_",gsub(c("-|:"),"",as.character(Sys.time()))), ".csv", sep=""), row.names=FALSE)
# 
# # max class by species
# maxclass <- data.frame(ELSeason=names(res$maxcls), hab=res$maxcls, row.names=NULL)
# maxclass <- merge(maxclass, lu_sgcn[c("ELSeason","SNAME","SeasonCode","SCOMNAME","TaxaDisplay" )], by="ELSeason")
# maxclass <- merge(maxclass, habitats_unq, by="hab")

###########################################
# make a pdf



lu_SpecificHabitatReq <- read.csv("lu_SpecificHabitatReq.csv", stringsAsFactors = FALSE)
lu_PrimaryMacrogroup <- read.csv("lu_PrimaryMacrogroup.csv", stringsAsFactors = FALSE)


##############################################################################################################
## Write the output document for the intro ###############
setwd(here::here("_data","output")) 
pdf_filename <- paste("a_",gsub(" ","_",gsub(c("-|:"),"",as.character(Sys.time()))),"_HabitatAssociationReport",sep="") # ,gsub("[^0-9]", "", Sys.time() )
makePDF("HabitatAssociationReport.rnw", pdf_filename) # user created function
deletepdfjunk(pdf_filename) # user created function # delete .txt, .log etc if pdf is created successfully.
setwd(here::here()) # return to the main wd
beepr::beep(sound=10, expr=NULL)
