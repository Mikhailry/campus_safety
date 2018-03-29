library(readxl)
#Be sure to edit path
IITenroll <- read_excel("~/Documents/Math 571 project/Data/SchoolEnrollment.xlsx", sheet = "IIT enroll")
UCIenroll <- read_excel("~/Documents/Math 571 project/Data/SchoolEnrollment.xlsx", sheet = "Uchi enroll")
View(IITenroll)
View(UCIenroll)