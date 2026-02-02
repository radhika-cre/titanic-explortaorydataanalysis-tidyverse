#### reading and writing the file ###
## get the working directory
getwd()
## set the working directory 
setwd("C:/Users/Admin/Documents/Semester 2/R program DSc/files_dowload")
getwd()
library(tidyverse)
## Reading CSV files
titanic <- read.csv("titanic_data.csv",header=T)

##structure of the data
str(titanic)
# first six records
head(titanic)


# Last six records
tail(titanic)

#Summary of the data 
summary(titanic)

# Selection specific columns
titanic$Survived #selection a single column

titanic[,c("Name","Gender","Fare")]#Selection multiple columns

titanic[,5:8] # selection columns by Index

## Filter Operations (row selection)
# Filter and selection passengers above the age of 35
titanic[titanic$Age > 35,c("PassengerId","Gender","Age")]
head(titanic[titanic$Age > 35,c("PassengerId","Gender","Age")]) # first few record

#Selection using SELECT - Select specific columns
sel_set_1 <- titanic %>% select(Pclass, Age, Fare, Survived)

#Female only
female_passenger <- titanic %>% 
  filter(Gender == "female")%>% select(Pclass, Age, Fare, Survived)

# Create new columns
#ifelse(condition,truth result,false result)
titanic$survival_status <- ifelse(titanic$Survived==0,"not survived" ,"survived")

# create a family count column using mutate()- to create a new column
titanic %>% mutate (FamilyMember=titanic$SibSp+titanic$Parch)%>% head()

#create an Adult / child column using age 
titanic %>% mutate (AgeGroup <- ifelse(titanic$Age>18,"Adult","child"))%>% head()
## what is feature engineering *
## sorting ####
# Sort by ascending fare # order
fares_asc <- titanic %>% arrange(Fare)

#sort by descending frequency
fares_dsc <- titanic %>% arrange(desc(Fare))

#sort by class then gnder then age
class_gender_age <- titanic %>% arrange(Pclass,Gender,Age)

#update the age of passenger id 1 to 23
# what is the current age of passenger id 1
titanic[titanic$PassengerId==1,"Age"] #22
titanic$Age[titanic$PassengerId==1]

#update it to 23
titanic$Age[titanic$PassengerId==1] <- 23
titanic$Age[titanic$PassengerId ==1]

# Group By
# Find the  number of male/female passenger
titanic %>% group_by(Gender)%>% summarise(Count = n())
titanic %>% group_by(Pclass)%>% summarise(PassengerByClass = n())
titanic %>% group_by(Gender)%>% summarise(avgage = mean(Age))# Na because few as na as value
titanic %>% group_by(Gender)%>% summarise(avgage = mean(Fare))

# count survivors by gender # group by works on categorical data
titanic %>% group_by(Gender)%>% summarise(NumSurvived= sum(Survived))
titanic %>% group_by(Pclass)%>% summarise(NumSurvived= sum(Survived))%>% arrange(NumSurvived)

# Writing data to file
write.csv(titanic,"Titanic_Modified.csv")

###############################################################################
## reading the txt files
text_data <- readLines("text_data.txt")
text_data

students <- read.table("student_marks.txt",header = T)
students

## writing to a text file 
student_data <- data.frame(
  Name = c("Adhishek","Mayuri","Arun Peter","Risha"),
  Age = c(20,21,19,22),
  Marks = c(85,90,78,88)
)
write.table(
  student_data,
  "StudentDetails.txt",
  sep="\t",
  row.names = F
)