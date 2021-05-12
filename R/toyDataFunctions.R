#
# library(tidyverse)
# # library(qdapDictionaries)
#
# ## Other distrubtions to play with
# # rlnorm(500,1,.6) %>% plot
#
# ## Define function for Date of Births
# DoB <- function(NumRecords,StartingDate="1940-12-30",EndingDate="2005-01-04"){
#   DoBs=seq(as.Date(StartingDate),as.Date(EndingDate), by="days")
#   Prob =  dnorm((1:length(DoBs))*4/length(DoBs) - 1)
#   sample(DoBs, NumRecords, replace=TRUE, prob=Prob)
# }
#
# ## Define function for Registration Dates
# RegistrationDates <- function(NumRecords,StartingDate="2018-01-01",EndingDate=Sys.Date()+365*3){
#   RegDates=seq(as.Date(StartingDate),as.Date(EndingDate), by="days")
#   Prob2 =  dnorm((1:length(RegDates))*4/length(RegDates) - 3)
#   sample(RegDates, NumRecords, replace=TRUE, prob=Prob2)
# }
#
# ## Define function for companies
# CompanyName <- function(NumRecords){
#   paste(toupper(sample(action.verbs,NumRecords,replace = T)),
#         sample(c("Ltd","Corp","Inc"),NumRecords,replace=T),
#         sep=" ")
# }
#
# BenRegMaker <- function(NN,PercentFemale) {
#   ## Ben Reg
#   BenReg <- data_frame(
#     DoB = DoB(NN),
#     sex = sample(c(0,1),NN,replace = T,prob = c(PercentFemale, (1 - PercentFemale))),
#     RegistrationDate = RegistrationDates(NN),
#     FistName = randomNames::randomNames()
#   )
#
#   ## Now assign names, Sex-appropriate
#   BenReg <- bind_cols(BenReg,
#                       randomNames::randomNames(NN, BenReg$sex, return.complete.data = T))
#   BenReg
# }
#
# gimmeNames <- function(NN, which = "first", dispOnly = T){
#   output <- noquote(randomNames::randomNames(NN, which.names = which))
#
#   if (dispOnly) {
#     cat("\n",noquote(paste0(output, "\n")))
#   } else {return(output)}
#
# }
#
# gimmeCountries <- function(){
#   df <- tibble::as_tibble(countrycode::codelist)
#   dplyr::select(df,country = country.name.en, iso2c, iso3c, continent)
# }
#
# generateToyProjectNames <- function(NN){
#   dplyr::sample_n(df,NN) %>% pull(country) %>% paste0(., " - ", NameCompany(NN,F),"\n") %>% noquote() %>% cat()
# }
#
# NameCompany <- function(NN,dispOnly = T){
#   adj <- praise::praise_parts$adjective
#   animalsCon <- file("./data/animalList.txt")
#   animals <- readLines(animalsCon)
#   close.connection(animalsCon)
#   output <- paste(
#     adj[sample(1:length(adj),NN,replace=TRUE)],
#     animals[sample(1:length(adj),NN,replace=TRUE)])
#   output <- tools::toTitleCase(output)
#   if (dispOnly) {
#     cat("\n",noquote(paste0(output, "\n")))
#   } else {return(output)}
# }
#
# FarmName <- function(NumRecords){
#   paste(toupper(sample(action.verbs,NumRecords,replace = T)),
#         sample(c("Farm","Organic Produce","Nursery"),NumRecords,replace=T),
#         sep=" ")
# }
#
# EventName <- function(NN){
#   paste(praise_parts$adverb[sample(1:length(praise_parts$adverb),NN,replace = TRUE)],
#         praise_parts$creating[sample(1:length(praise_parts$creating),NN,replace = TRUE)],
#         c("Hotels","Fields","Farmer Schools", "Towns","Streets","Food")[sample(
#           1:length(c("Hotels","Fields","Farmer Schools", "Towns","Streets","Food")),NN,replace = TRUE)])
# }
