#The below code reads PAC and PM clinical reports and creates a formatted clinical database
#for usage within INOVIO's Immunology and Biomarkers group - HPV-301 Program

#' CreateClinicalDatabase
#' CreateClinicalDatabase reads PAC and PM clinical reports and creates a
#' formatted clinical database for usage within INOVIO's Immunology and
#' Biomarkers group - HPV-301 Program
#'
#' @returnA data frame with PAC and PM data read from the current working
#'   directory
#' @export
#'
#'
CreateClinicalDatabase <- function(){

filesToAdd <- list.files(pattern = "*.pdf", ignore.case = TRUE, recursive = TRUE)
files <- filesToAdd[filesToAdd %>% grep(pattern = "PAC")]


for(j in 1:(files %>% length)){
  # read pdf into R Enviroment####
  PacReport <- pdf_text(files[j], upw = "HPV-301") %>% toString()

  #extract Data of interest####
  sid <- PacReport %>% str_extract("(?<=Subject).*RO.{7}") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("\\s")

  clinSite <- sid %>% str_extract("RO.{4}")

  date <- PacReport %>% str_extract("Collection Date:(.*?)[:letter:]") %>%
    str_remove_all("[[:alpha:]]") %>%
    str_remove_all(":") %>%
    str_remove_all("\\s")

  visit <- PacReport %>% str_extract("(?<=Visit).*") %>% toupper %>%
    str_remove_all("\\s") %>%
    str_remove_all("[[:punct:]]")


  OverAllDiagnosis <- PacReport %>% str_extract("(?<=Overall Diagnosis:\r\n).*") %>% trimws()



  pacDataTemp <- PacReport %>% str_extract_all(regex("(PAC).*Gross", dotall = TRUE)) %>%
    toString() %>%
    str_replace_all(pattern = "\n\n","\n")

  if(pacDataTemp %>% str_extract_all(".*Case Comment.*") %>% nchar > 12){
    pacDataTemp <- PacReport %>% str_extract_all(regex("(PAC).*Case", dotall = TRUE)) %>%
      toString() %>%
      str_replace_all(pattern = "\n\n","\n")}


  pacData <- pacDataTemp %>% str_extract_all("([:upper:]:).*\r\n.*\r") %>% as.data.frame(stringsAsFactors = FALSE)


  ####correct LEEP reporting
  if(pacDataTemp %>% str_extract_all(".*LEEP/LLETZ.*") %>% nchar > 12){
    pacDataLeep <- pacDataTemp %>% str_extract(".*LEEP/LLETZ.*") %>%  trimws
    pacData[1,] <-paste((paste("A:", pacDataLeep, sep = " ")), OverAllDiagnosis, sep = "\n")
    rm(pacDataLeep)}


  if(pacData %>% nchar < 13)( pacData[1,] <- "ERROR\nERROR")

  for(t in 1:(pacData %>% nrow)){
    TempPacString <- str_split(pacData[t,], "\n",n=2) %>% as.data.frame()
    ifelse((TempPacString[2,] %>% as.vector() %>% nchar) >2,
           pacData[t,] <- pacData[t,],
           {SavePaCData <- pacDataTemp %>% str_extract_all("(?<=[:upper:]:).*\n\n.*\n") %>% as.data.frame()
           pacData[t,] <- gsub("\n\n","\n",SavePaCData[1,])
           })
  }

  #create output document####

  for(i in 1:(nrow(pacData))){

    if(exists("OADTemp")) rm(OADTemp)
    OADTemp <- c(OverAllDiagnosis, (rep("-", (nrow(pacData)-1))))

    outputTemp <- cbind(sid,
                        clinSite,
                        date,
                        visit,
                        str_split((pacData[i,] %>% as.character),"\n", n=2),
                        OADTemp[i]) %>%
      as.data.frame

    outputTemp <- outputTemp %>% unlist() %>% t()

    #Add step to call error if colnumbers dont match

    if(exists("output")){
      if(outputTemp %>% ncol != output %>% ncol){

        colFix <- output %>% ncol - outputTemp %>% ncol
        for (t in 1: (colFix %>% length)){
          outputTemp <- cbind(outputTemp, "ERROR")
        }}}


    ### adding above functionality

    ifelse(exists("output"),
           #this is throwing an error for some reason
           output <- rbind(output, outputTemp),
           output <- outputTemp)
  }}

colnames(output) <- c("SUBJECT", "CLINICALSITE", "DATE","VISIT", "PAC_SAMPLING_SITE", "PAC_DIAGNOSIS","OVERALL_DIAGNOIS")
output <- output %>% trimws %>% as.data.frame(stringsAsFactors = FALSE)

#adding in PM data####


files <- list.files(pattern = ".*PM.*.pdf", ignore.case = TRUE, recursive = TRUE)

####

for(j in 1:(files %>% length)){
  # read pdf into R Enviroment####
  PacReport <- pdf_text(files[j], upw = "HPV-301") %>% toString()

  #extract Data of interest####
  sid <- PacReport %>% str_extract("(?<=Subject).*RO.{7}") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("\\s")

  visit <- PacReport %>% str_extract("(?<=Visit).*") %>% toupper %>%
    str_remove_all("\\s") %>%
    str_remove_all("[[:punct:]]")

  pacDataTemp <- PacReport %>% str_extract_all(regex("Patient Management Diagnosis:.*Gross", dotall = TRUE)) %>%
    toString() %>%
    str_replace_all(pattern = "\n\n","\n")


  #capture the Letter header (A: , B:, etc.)
  pacData <- pacDataTemp %>% str_extract_all("([:upper:]:).*\r\n.*\r") %>% as.data.frame(stringsAsFactors = FALSE)


  if(pacDataTemp %>% str_extract_all(".*LEEP/LLETZ.*") %>% nchar > 12){
    pacData <- pacDataTemp %>% str_extract(".*LEEP/LLETZ.*\r\n.*\r") %>%  trimws %>% as.data.frame(stringsAsFactors = FALSE)}


  if(pacData %>% nchar < 13)( pacData[1,] <- "ERROR\nERROR")

  for(t in 1:(pacData %>% nrow)){
    TempPacString <- str_split(pacData[t,], "\n",n=2) %>% as.data.frame()
    ifelse((TempPacString[2,] %>% as.vector() %>% nchar) >2,
           pacData[t,] <- pacData[t,],
           {SavePaCData <- pacDataTemp %>% str_extract_all("(?<=[:upper:]:).*\n\n.*\n") %>% as.data.frame()
           pacData[t,] <- gsub("\n\n","\n",SavePaCData[1,])
           })
  }

  #create output document####

  for(i in 1:(nrow(pacData))){

    outputTemp <- cbind(sid,
                        visit,
                        str_split((pacData[i,] %>% as.character),"\n", n=2)) %>%
      as.data.frame

    outputTemp <- outputTemp %>% unlist() %>% t()

    if(exists("PMoutput")){
      if(outputTemp %>% ncol != PMoutput %>% ncol){

        colFix <- PMoutput %>% ncol - outputTemp %>% ncol
        for (t in 1: (colFix %>% length)){
          outputTemp <- cbind(outputTemp, "ERROR")
        }}}


    ifelse(exists("PMoutput"),
           PMoutput <- rbind(PMoutput, outputTemp),
           PMoutput <- outputTemp)
  }}

PMoutput <- PMoutput %>% trimws %>% as.data.frame(stringsAsFactors = FALSE)
colnames(PMoutput) <- c("SID","VISIT","SAMPLE_SITE","PM_DIAGNOSIS")

####Merge PACOutPut with PMOutPut
PMoutput$TARGET <-  paste(PMoutput$SID, PMoutput$VISIT, PMoutput$SAMPLE_SITE)
PMMerge <- PMoutput[,c("TARGET", "PM_DIAGNOSIS")]

output$TARGET <- paste(output$SUBJECT, output$VISIT, output$PAC_SAMPLING_SITE)

mergedOutputs <- merge(x = output, y = PMMerge, by= "TARGET", all.x = TRUE, all.y = TRUE, no.dups = TRUE)



for( t in 1:(mergedOutputs %>% nrow)){
  if(mergedOutputs$SUBJECT[t]%>% is.na){
    (mergedOutputs$SUBJECT[t] <- str_split(mergedOutputs$TARGET[t], pattern = " ", n=3, simplify = TRUE)[1])
    (mergedOutputs$VISIT[t] <- str_split(mergedOutputs$TARGET[t], pattern = " ", n=3, simplify = TRUE)[2])
    (mergedOutputs$PAC_SAMPLING_SITE[t] <- str_split(mergedOutputs$TARGET[t], pattern = " ", n=3, simplify = TRUE)[3])
  }}

ClinicalDBAddOn <<- mergedOutputs

}
