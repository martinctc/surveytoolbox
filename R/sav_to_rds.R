#' Serialise a SPSS file to RDS
#' 
#' This function is useful for converting large, bulky SAV files into RDS files which are
#' faster to load and take up less memory. Use readRDS() for loading in the exported file.
#' By default, the old file name is retained and saved in the same location as RDS
#' unless otherwise specified by the export argument.
#' 
#' @param import String containing path to .SAV file
#' @param export String containing desired file name for RDS export. 
#' Default is to use same name as .SAV file     
#' @import haven   
#' @export
sav_to_rds<-function(import,export=""){
  p <- haven::read_sav(import)
  export<-ifelse(export=="",
                 str_replace_all(import,".sav",".rds"),
                 export)
  saveRDS(p,file=export)
}       