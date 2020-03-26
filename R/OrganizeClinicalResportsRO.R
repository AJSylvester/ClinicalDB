#Processing Clinical Reports received by Immunology
#files(.pdf, .zip, etc.) uploaded via email
#code to extract, sort by program and rename(PAC, PM, DDL, DCL, etc.)

#' OrganizeClinicalReportsRO
#' OrganizeClinicalReportsRO renames and arranges .pdfs of PAC, PM, and Virology
#' clinical reports from HPV-301
#'
#' @return Renames and organizes files in the working directory
#' @export
#'
OrganizeClinicalReportsRO <- function(){

Masterfiles <- list.files()
w <- getwd()
#Reval One (RO) files####
# {
RoFiles <- Masterfiles %>% toupper %>% str_extract(".*RO.*.PDF") %>% na.omit




if(RoFiles %>% length > 0){
  if(!(dir.exists(file.path(w,paste("RO", Sys.Date(), sep = "-")))))(dir.create(file.path(w,paste("RO", Sys.Date(), sep = "-"))))
  if(!(dir.exists(file.path(w,paste("PROCESSED", Sys.Date(), sep = "-")))))(dir.create(file.path(w,paste("PROCESSED", Sys.Date(), sep = "-"))))

    for (i in 1:(RoFiles %>% length)){
      file.copy(file.path(w,RoFiles[i]),file.path(w,paste("RO", Sys.Date(), sep = "-")))
      file.copy(file.path(w,RoFiles[i]),file.path(w,paste("PROCESSED", Sys.Date(), sep = "-")))
      file.remove(file.path(w,RoFiles[i]))
    }
  }

#Rename files as apporiate
setwd(file.path(w,paste("RO", Sys.Date(), sep = "-")))
v <- getwd()
files <- list.files(pattern = ".*RO.*.pdf", ignore.case = TRUE, recursive = FALSE)

          for(j in 1:(files %>% length)){
            # read pdf into R Enviroment
            PacReport <- pdf_text(files[j], upw = "HPV-301") %>% toString()

            sid <- PacReport %>% str_extract("(?<=Subject).*RO.{7}") %>%
              str_remove_all("[[:punct:]]") %>%
              str_remove_all("\\s")

            visit <- PacReport %>% str_extract("(?<=Visit).*") %>% toupper %>%
              str_remove_all("\\s") %>%
              str_remove_all("[[:punct:]]")

            PAC <- PacReport %>% str_extract("Pathology Adjudication Committee")
            PM <- PacReport %>% str_extract("Patient Management Diagnosis")

            if(PAC %>% na.omit %>% length > 0) file.rename(file.path(v,files[j]),file.path(v,paste(sid,visit,"PAC.pdf", sep = " ")))

            if(PM %>% na.omit %>% length > 0) file.rename(file.path(v,files[j]),file.path(v,paste(sid,visit,"PM.pdf", sep = " ")))
}
}


