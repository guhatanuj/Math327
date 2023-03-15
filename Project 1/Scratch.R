library(tidyverse)
library(readxl)
library(xlsx)
library(datadictionary)
colleges = read_xlsx("Colleges.xlsx")
colleges <- colleges %>% 
  rename("ID" = "Unique identification number of the institution",
         "ACT" = "ACT Composite 75th percentile score",
         "GPA" = "Secondary school GPA",
         "CollegePrep" = "Completion of college-preparatory program",
         "TOEFL" = "TOEFL (Test of English as a Foreign Language",
         "NumberEnrolled" = "Enrolled total",
         "ACTPercentage" = "Percent of first-time degree/certificate-seeking students submitting ACT scores",
         "SATWR" = "SAT Evidence-Based Reading and Writing 75th percentile score",
         "SATM"= "SAT Math 75th percentile score")

qqnorm(colleges$GPA)
plot(colleges$GPA)


as.character(colleges$TOEFL)
colleges$TOEFL[colleges$TOEFL=="1"]<-"Required"
colleges$TOEFL[colleges$TOEFL=="2"]<-"Recommended"
colleges$TOEFL[colleges$TOEFL=="3"]<-"Neither required nor recommended"
colleges$TOEFL[colleges$TOEFL=="5"]<-"Considered but not required"

