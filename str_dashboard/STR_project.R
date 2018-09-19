#Objective is to get page 7,8,and,9 out of the data. Specifically Sun - Sat in the top 25 Markets. 
#From here I would like to make a dashboard with the data. More to come...
#test subject is STR

# Using Libray pdftools
library(pdftools)
library(stringr)
library(plotly)
library(dplyr)

#getting page 7 from the file 
get_file_from_date <- function(start_date) {
  filename <- paste(start_date, ".pdf", sep = "")
  file_text <- pdf_text(filename)
  # Want to focus on page 3 of the vector in the list.
  page7 <- file_text[[7]]
  #Using regexpr to find the location of RevPAR\n
  tindex <- regexpr("Running\nTop 25 Markets", page7)
  #deleting "Running\nTop 25 Markets" and everything before it using substring
  page7 <- substring(page7,tindex+23)
  # String split with "\n" - creates new lines
  newline <- strsplit(page7, "\n")
  #using substring turned my page into a list, I need it to be a character
  newchar <- unlist(newline)
  #taking out all of the rows I dont need.
  newchar <- newchar[-(1)]
  newchar <- newchar[-(26:28)]
  # Extracted all of the floats from the vectors
  extract <- (str_extract_all(newchar,"[+-]?([0-9]*[.])?[0-9]+"))
  # turning the list back into a character
  un_list <- as.numeric(unlist(extract))
  # telling it there need to be 18 lines
  rep_list <- rep(1:18, times=length(un_list)/18)
  # I dont know whats going on here
  split_list <- split(un_list, rep_list)
  # making the columns
  df <- cbind.data.frame(split_list, stringsAsFactors=F)
  # taking out the columns that I dont want
  df <- df[-(8:18)]
  #adding column names
  row.names(df) <- c("Anaheim/SantaAna_CA","Atlanta_GA","Boston_MA","Chicago_IL","Dallas_TX","Denver_CO","Detroit_MI","Houston_TX",
                     "LosAngeles_LongBeach_CA","Miami_FL","Minneapolis_StPaul_MN-WI","Nashville_TN","NewOrleans_LA",
                     "New York_NY","Norfolk_VirginiaBeach_VA","Oahu_HI","Orlando_FL","Philadelphia_PA","Phoenix_AZ","SanDiego_CA",
                     "SanFrancisco_SanMateo_CA","Seattle_WA","StLouis_MO.IL","Tampa_StPetersburg_FL","Washington_DC.MD.VA")
  # Creating row names
  #names(df) = daysofweek
  tran_df <- t(df)
  # turning the transposed data back to a dataframe (it turned into a matrix when I transposed it. boo.)
  tran_df <- as.data.frame(tran_df)
  # Creating a new column called DayofWeek
  tran_df["DayofWeek"] <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  tran_df <- tran_df[colnames(tran_df)[c(26,1:25)]]
  # working on adding a date column
  #creates a range of dates from the starting date to 6 days after
  tran_df["Date"] <- c(seq(from = as.Date(start_date), to = as.Date(start_date)+6, by = 'day'))
  # moving the new date column to the front.
  tran_df <- tran_df[colnames(tran_df)[c(27,1:26)]]
  return(tran_df)
}

#These are all of the files!!!
Dec31.17_Jan6 <- get_file_from_date("2017-12-31")
Jan7_13 <- get_file_from_date("2018-01-07")
Jan14_20 <- get_file_from_date("2018-01-14")
Jan21_27 <- get_file_from_date("2018-01-21")
Jan28_Feb3 <- get_file_from_date("2018-01-28")
Feb4_10 <- get_file_from_date("2018-02-04")
Feb11_17 <- get_file_from_date("2018-02-11")
Feb18_24 <- get_file_from_date("2018-02-18")
Feb25_Mar3 <- get_file_from_date("2018-02-25")
Mar4_10 <- get_file_from_date("2018-03-04")
Mar11_17 <- get_file_from_date("2018-03-11")
Mar18_24 <- get_file_from_date("2018-03-18")
Mar25_31 <- get_file_from_date("2018-03-25")
Apr1_7 <- get_file_from_date("2018-04-01")
Apr8_14 <- get_file_from_date("2018-04-08")
#Apr15_21 <- get_file_from_date("")
#Apr22_28 <- get_file_from_date("")
#Apr29_May5 <- get_file_from_date("")
May6_12 <- get_file_from_date("2018-05-06")
May13_19 <- get_file_from_date("2018-05-13")
May20_26 <- get_file_from_date("2018-05-20")
May27_Jun2 <- get_file_from_date("2018-05-27")
Jun3_Jun9 <- get_file_from_date("2018-06-03")
Jun10_Jun16 <- get_file_from_date("2018-06-10")
Jun17_Jun23 <- get_file_from_date("2018-06-17")
Jun24_Jun30 <- get_file_from_date("2018-06-24")
Jul1_7 <- get_file_from_date("2018-07-01")
#Jul8_14 <- get_file_from_date("")
Jul15_21 <- get_file_from_date("2018-07-15")
#Jul22_28 <- get_file_from_date("")
Jul29_Aug4 <- get_file_from_date("2018-07-29")
Aug5_11 <- get_file_from_date("2018-08-05")
Aug12_18 <- get_file_from_date("2018-08-12")
Aug19_24 <- get_file_from_date("2018-08-19")
Aug25_Sept1 <- get_file_from_date("2018-08-25")
Sept2_Sept8 <- get_file_from_date("2018-09-02")

#binding dem rows 
Hotel_Occ <- rbind(Dec31.17_Jan6,Jan7_13,Jan14_20,Jan21_27,Jan28_Feb3,Feb4_10,Feb11_17,
      Feb18_24,Feb25_Mar3,Mar4_10,Mar11_17,Mar18_24,Mar25_31,Apr1_7,Apr8_14,
      May6_12,May13_19,May20_26,May27_Jun2,Jun3_Jun9,Jun10_Jun16,
      Jun17_Jun23,Jun24_Jun30,Jul1_7,Jul15_21,Jul29_Aug4,Aug5_11,Aug12_18,Aug19_24,Aug25_Sept1,Sept2_Sept8)

write.csv(Hotel_Occ, "HotelOccupancy.csv")

