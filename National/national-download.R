# download national YRBSS data files and user guidels for 1991, 1993, ..., 2019
# all files retrieved 2020-12-16

library(purrr)
library(here) # use base function file.path() in publication version

# files for 1991, 1993, ..., 2013 are in ftp; those from 2015 on are https
ftp_seq <- seq(1991, 2013, 2)
# download data text files
# https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
data_url_list <- c(
  "https://www.cdc.gov/healthyyouth/data/yrbs/files/2019/XXH2019_YRBS_Data.dat",
  "https://www.cdc.gov/healthyyouth/data/yrbs/files/2017/XXH2017_YRBS_Data.dat",
  "https://www.cdc.gov/healthyyouth/data/yrbs/files/yrbs2015.dat",
  paste0("ftp://ftp.cdc.gov/pub/data/yrbs/", ftp_seq, "/YRBS", 
         ftp_seq, ".dat"))
datafile_lst <- paste0("YRBS", seq(1991, 2019, 2), ".dat")
walk2(data_url_list, datafile_lst, 
      function(x, y) download.file(x, y, mode = "wb"))
      # function(x, y) download.file(x, here("National", y), mode = "w")) 

# function not working; extraneous extra characters
# this works
download.file("ftp://ftp.cdc.gov/pub/data/yrbs/1991/YRBS1991.dat", "test2.txt",
              mode = "wb")
# NOT DOWNLOADING PROPERLY FROM CUSTOM FUNCTION--CHECK OUT


# download user guides
pdf_url_lst <- c(
    "https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2019/2019_National_YRBS_Data_Users_Guide.pdf",
    "https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2017/2017_YRBS_Data_Users_Guide.pdf",
    "https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2015/2015_yrbs-data-users_guide_smy_combined.pdf",
    paste0("ftp://ftp.cdc.gov/pub/data/yrbs/", ftp_seq,
           "/YRBS_", ftp_seq, "_National_User_Guide.pdf"))
pdf_lst <- c("2019_National_YRBS_Data_Users_Guide.pdf",
             "2017_YRBS_Data_Users_Guide.pdf",
             "2015_yrbs-data-users_guide_smy_combined.pdf",
             paste0("YRBS_", ftp_seq, "_National_User_Guide.pdf"))
walk2(pdf_url_lst, pdf_lst, 
      function(x, y) download.file(x, here("National", y), mode = "wb"))            
             
  
  
  



