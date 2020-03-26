#The below code reads Virology Reports reports and creates a formatted virology database
#for usage within INOVIO's Immunology and Biomarkers group - HPV-301 Program

#'CreateVirologyDataBase
#'CreateVirologyDataBase reads virology Reports reports and creates a formatted
#'virology database for usage within INOVIO's Immunology and Biomarkers group -
#'HPV-301 Program
#'
#'
#'@return A data frame with virology data read from the current
#'  working directory
#'@export
#'
CreateVirologyDatabase <- function(){

  #Remove any files that may be named as a PM or PAC read
  filesToAdd <- list.files(pattern = "*.pdf", ignore.case = TRUE, recursive = TRUE)
  files <- filesToAdd[-(filesToAdd %>% grep(pattern = "PM|PAC|*.Patient.*|.*Mana.*|*.MANA.*|.*mana.*|.*Remove.*|.*remove.*|.*Ship.*"))]

  for(j in 1:(files %>% length)){

    DDLReport <- pdf_text(files[j]) %>% toString()

    sid <- DDLReport %>% str_extract("(?<=Subject ID:).*RO.{7}") %>% trimws
    clinSite <- sid %>% str_extract("RO.{4}")
    date <- DDLReport %>% str_extract("(?<=Collection Date:).*") %>% trimws
    visit <- DDLReport %>% str_extract("(?<=Visit ID:).*") %>% toupper %>% trimws
    # cytology <- DDLReport %>% str_extract("(?<=General categorization).*") %>% trimws
    #   cytology %>% str_extract(".*Negative.*") %>% is.na
    cytology <- DDLReport %>% str_extract("(?<=Squamous cell abnormalities).*") %>% trimws

    if(cytology %>% is.na)(cytology <- "NA")

    if(cytology %>% tolower == "not applicable"){
      cytology <- paste(cytology,
                        DDLReport %>% str_extract("(?<=Notes).*") %>% trimws,
                        sep = "_")}

    hpv16 <- DDLReport %>% str_extract("(?<=COBAS HPV 16 genotyping).*") %>% toupper %>% trimws
    hpv18 <- DDLReport %>% str_extract("(?<=COBAS HPV 18 genotyping).*") %>% toupper %>% trimws
    other <- DDLReport %>% str_extract("(?<=COBAS other high risk HPV types).*") %>% toupper %>% trimws

    #if different report format

    if(sid %>% nchar %>% is.na){

      sid <- DDLReport %>% str_extract(regex("(?<=SUBJECT ID).*RO.{7}", dotall = TRUE))%>% str_extract("RO.{7}")
      clinSite <- sid %>% str_extract("RO.{4}")
      date <- DDLReport %>% str_extract(regex("(?<=COLLECTION DATE).*(?=AGE)", dotall = TRUE)) %>% trimws %>% str_sub(start = -12) %>% trimws
      visit <- DDLReport %>% str_extract(regex("(?<=VISIT ID).*(?=PAP)", dotall = TRUE)) %>% str_extract("(?<=[:digit:]{2}).*") %>% trimws %>% toupper
      # cytology <- DDLReport %>% str_extract("(?<=General categorization: ).*") %>% trimws
      cytology <- DDLReport %>% str_extract ("(?<=Squamous cell abnormalities:).*")%>% trimws

      hpv16 <- DDLReport %>% str_extract("(?<=COBAS HPV 16 genotyping:).*") %>% toupper %>% trimws
      hpv18 <- DDLReport %>% str_extract("(?<=COBAS HPV 18 genotyping:).*") %>% toupper %>% trimws
      other <- DDLReport %>% str_extract("(?<=COBAS other high risk HPV types:).*") %>% toupper %>% trimws
    }

    #if pdf is scanned image
    if(sid %>% nchar %>% is.na){
      DDLReport <- pdf_ocr_text(files[j]) %>% toString()
      sid <- DDLReport %>% str_extract(regex("(?<=SUBJECT ID).*RO.{7}", dotall = TRUE))%>% str_extract("RO.{7}")
      clinSite <- sid %>% str_extract("RO.{4}")
      date <- DDLReport %>% str_extract(regex("(?<=COLLECTION DATE).*(?=AGE)", dotall = TRUE)) %>% trimws %>% str_sub(start = -12) %>% trimws
      visit <- DDLReport %>% str_extract(regex("(?<=VISIT ID).*(?=PAP)", dotall = TRUE)) %>% str_extract("(?<=[:digit:]{2}).*") %>% trimws %>% toupper
      #cytology <- DDLReport %>% str_extract("(?<=General categorization: ).*") %>% trimws

      cytology <- DDLReport %>% str_extract ("(?<=Squamous cell abnormalities:).*")%>% trimws



      hpv16 <- DDLReport %>% str_extract("(?<=COBAS HPV 16 genotyping:).*") %>% toupper %>% trimws
      hpv18 <- DDLReport %>% str_extract("(?<=COBAS HPV 18 genotyping:).*") %>% toupper %>% trimws
      other <- DDLReport %>% str_extract("(?<=COBAS other high risk HPV types:).*") %>% toupper %>% trimws
    }



    #create output document

    outputTemp <- cbind(sid,
                        clinSite,
                        date,
                        visit,
                        cytology,
                        hpv16,
                        hpv18,
                        other) %>% as.data.frame

    #Add step to call error if colnumbers dont match

    if(exists("output")){
      if(outputTemp %>% ncol != output %>% ncol){

        colFix <- output %>% ncol - outputTemp %>% ncol
        for (t in 1: (colFix %>% length)){
          outputTemp <- cbind(outputTemp, "ERROR")
        }}}


    ifelse(exists("output"),
           output <- rbind(output, outputTemp),
           output <- outputTemp)
  }


  colnames(output) <- colnames(output) %>% toupper

  output$CYTOLOGY <- output$CYTOLOGY %>% gsub(pattern = "Not present", replacement = "Negative for Intraepithelial Lesion or Malignancy (NILM)")

  for (i in 6:8){
    output[,i] <- output[,i] %>% gsub(pattern = ".*NOT.*", replacement = "NEG")
    index <- grep(pattern = "NEG", x = output[,i])
    output[-index,i] <- "POS"
  }

    VirologyDBAddOn <<- output
}
