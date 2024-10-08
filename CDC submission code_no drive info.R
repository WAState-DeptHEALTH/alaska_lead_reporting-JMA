## Prep for CDC reporting
library(stringi)
library(stringr)
library(readr)
library(data.table)
library(dplyr)
library(haven)
library(httr)
library(XML)
library(rjson)
library(raster)
library(rgeos)
library(tidyverse)

#Add most recent registry
network_drive<-Sys.getenv('network_drive')
final <- read.csv(file.path(network_drive,'quarter data.csv'))

#Filter for the months and year you are reporting
final$test.month <- stri_pad_left(final$test.month, 2, "0")
final$CDC_Child_ID <- stri_pad_left(final$CDC_Child_ID, width = 8, pad = 0)
df <- final %>% filter((test.month == "04" | test.month == "05" | test.month == "06")  & test.year == "2024")
df$CDC_FIPS <- stri_pad_left(df$CDC_FIPS, 3, pad = "0")
df$CDC_PTRace<-as.character(df$CDC_PTRace)
df$CDC_ptSex <- ifelse(is.na(df$CDC_ptSex), "9", df$CDC_ptSex)
##CHI string - 1 instance of each child ID, reformat race variable
df.chi <- df %>% subset(!(duplicated(CDC_Child_ID))) %>%
  mutate(RACE_AIAN = ifelse(CDC_PTRace == "1", 1, 2),
         RACE_ASIAN = ifelse(CDC_PTRace == "2", 1, 2),
         RACE_BLACK = ifelse(CDC_PTRace == "3", 1, 2),
         RACE_NHOPI = ifelse(CDC_PTRace == "4", 1, 2),
         RACE_WHITE = ifelse(CDC_PTRace == "5", 1, 2),
         RACE_OTHER = ifelse(CDC_PTRace == "6", 1, 2),
         RACE_RTA = 2,
         RACE_UNKNOWN = ifelse(CDC_PTRace == "9", 1, 2))

##Create string
##Change digit after A to correct quarter and update next two digits to current year if needed
chi <- paste0("CHIA22453100", df.chi$CDC_Child_ID, df.chi$CDC_DOB, df.chi$CDC_ptSex, df.chi$CDC_PtEthnicity, 
              " 9999999993", df.chi$RACE_AIAN, df.chi$RACE_ASIAN, df.chi$RACE_BLACK, df.chi$RACE_NHOPI, 
              df.chi$RACE_WHITE, df.chi$RACE_OTHER, df.chi$RACE_RTA, df.chi$RACE_UNKNOWN)

##Check that all rows are the same number of characters. Should be 49
table(nchar(chi))              

##ADD string
library(tidyr)
library(stringi)
library(stringr)
df$CDC_zip<-as.numeric(df$ZIPCODE)
df<-df%>%
  mutate(CDC_zip=coalesce(CDC_zip,00000))
df$CDC_FIPS <- stri_pad_left(df$CDC_FIPS, 3, pad = "0")
#Make sure add ID is 8 digits, city is 15 digits, zip is 5 digits, drop blank or NA address ID's
df.add <- df %>% 
  mutate(CDC_AddID = stri_pad_left(CDC_AddID, width = 8, pad = "0"),
         CDC_zip = stri_pad_left(CDC_zip, width = 5, pad = "0"),
         CDC_city = stri_pad_right(CITY, width = 15, pad = " "),
         CDC_zip = ifelse(nchar(CDC_zip) == 0, "     ", CDC_zip)) %>%
  subset(!(duplicated(CDC_AddID)& !(is.na(CDC_AddID))))
table(nchar(df.add$CDC_city))
df.add$CDC_city=stri_pad_right(df.add$CDC_city, width = 15)
df.add$CDC_city<-str_trunc(df.add$CDC_city, 15,"right")
df.add$CDC_city<-toupper(df.add$CDC_city)
table(is.na(df.add$CDC_AddID))
head(df.add$CDC_city)
table(duplicated(df.add$CDC_AddID))
df.add<-df.add %>% drop_na("CDC_AddID")


##Create string
##Change digit after A to correct quarter and update next two digits to current year if needed
add <- paste0("ADDA22453100", df.add$CDC_AddID, df.add$CDC_city, df.add$CDC_FIPS, df.add$CDC_zip, "0000", "WA", "       9                ")

##MAke sure every row is the same number of digits. Should be 73
table(nchar(add))                            


##LAB string  

df.lab <- df %>% 
  #select one result / kid / day using DailyResult field
  filter(DailyResult==1) %>% 
  mutate(CDC_AddID = stri_pad_left(AddID, width = 8, pad = "0"),
         
         #format result field to 2 decimal places, trim white space, make length 6 characters
         CDC_result = format(round(result_num_final, digits = 2), nsmall = 2),
         CDC_result = trimws(CDC_result),
         CDC_result = stri_pad_left(CDC_result, width = 6, pad = "0"),
         #define interpreter field by looking for > and < in the original result field
         CDC_result_int = case_when(stri_detect_fixed(result_text_final, "<") ~ 2,
                                    stri_detect_fixed(result_text_final, ">") ~ 3,
                                    TRUE ~ 1)) 
dflabtest<-df.lab
table(is.na(dflabtest$CDC_AddID))
df.lab$CDC_AddID<-stri_pad_left(df.lab$AddID, width = 8, pad = "0")
df.lab$PERFORMING_ORG_NAME2 <- ifelse(is.na(df.lab$PERFORMING_ORG_NAME), " ", df.lab$PERFORMING_ORG_NAME)
df.lab$PERFORMING_ORG_NAME<-toupper(df.lab$PERFORMING_ORG_NAME)
df.lab$SenderName2<-substr(df.lab$PERFORMING_ORG_NAME2, 1,43)
df.lab$SenderName2<-stri_pad_right(df.lab$SenderName2, width=43, pad=" ")
df.lab$SenderName3<-str_trunc(df.lab$SenderName2, 43,"right")

table(nchar(df.lab$SenderName2))
df.lab$CDC_AddID<- df.lab$CDC_AddID %>% replace_na("00000000")

##Create string
##Change digit after A to correct quarter and update next two digits to current year if needed
lab1 <- paste0("LABA22453100", df.lab$CDC_Child_ID, df.lab$CDC_TestDate, df.lab$CDC_AddID, "9", "  ", "9", 
               df.lab$CDC_SampleTy, "9999000.00", "                ", df.lab$CDC_result, df.lab$CDC_result_int,
               "000.00",df.lab$SenderName2,"                     ")

##Check that all rows are the same number of characters. Should be 144

table(nchar(lab1))


##Write CSVs - change file paths as needed
write.csv(chi, "INSERT OUTPUT ADDRESS HERE", row.names = FALSE)
write.csv(add, "INSERT OUTPUT ADDRESS HERE", row.names = FALSE)
write.csv(lab1, "INSERT OUTPUT ADDRESS HERE", row.names = FALSE)
##Convert CSVs to text files for upload