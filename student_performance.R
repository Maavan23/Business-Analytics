getwd()

Student_data = read.csv('student_performance.csv',header = TRUE, sep=",")
Student_data

#Central Tendency
#measyres of location
# 1) Mean (add all values and divide by how many values there are)
#ex 6,7,8
#Mean = (6+7+8)/3 = 7
mean(student_performance$Final_Exam_Score)

# 2) Median (put numbers in order and pick the middle one)
median(student_performance$Final_Exam_Score)

# 3) Mode(Most repeated value)
#Find the mode
mode_function <-function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

mode_function(student_performance$Final_Exam_Score)

#Measures of Dispersion(Range , Variance, Standard Deviation,
#IQR,Box plots)
#means how spread out the data is (Measure data variability)
getwd()

Student_data = read.csv('student_performance.csv',header = TRUE, sep=",")
Student_data

#Calculate Range
range_marks <- max(student_performance$Final_Exam_Score) - min(student_performance$Final_Exam_Score)
range_marks

#Calculate IQR Interquartile Range
iqr_marks <- IQR(student_performance$Final_Exam_Score)
iqr_marks

#Variance and Standard Deviation
var_marks <- var(student_performance$Final_Exam_Score)
var_marks

sd_marks <- sd(student_performance$Final_Exam_Score)
sd_marks

#ANOVA TEST
result <- aov(Hours_Studied ~ Assignment_Score, data = Student_data)
summary(result)
#              Df  Sum Sq  Mean Sq  F value Pr(>F)
#Gender        1   144     144.1    0.429   0.514
#Residuals    98   32936   336.1
#if Pr(>F) < 0.05 ==> Statically Significant
#the average marks between Males and Females are Signficantly different.

#Frequency Distribution and Interval and midpoint
#1 create class intervals
breaks <- seq(0, 100, by = 10) #creates ex :- 0-10,10-20

#2 cut marks into Intervals
student_performance$Interval <- cut(student_performance$Final_Exam_Score, breaks = breaks, right = FALSE)

#3 Frequency table
freq_table <- table(student_performance$Interval)
freq_table

#Midpoint
midpoints <- (head(breaks, -1)) + tail(breaks, -1) /2
midpoints

#Create Histogram

barplot(freq_table,
        main = "Histogram of student_performance",
        xlab = "Marks Range",
        ylab = "Number of Students",
        col = "lightblue",
        boarder = "black")

#Frequency Polygon (Midpoint & Lines)
#Midpoints 
midpoints <- (head(breaks, -1) + tail(breaks, -1))/2

#convert frequency table to numaric vectors
freq_values <- as.numeric(freq_table)

#Convert frequency polygon
plot(midpoints, freq_values, type = "o",
     main = "Frequency Polygon",
     xlab = "Midpoint of Marks Range",
     ylab = "Number of Students",
     pch = 16)

#Bell Curve
#1 Load data
getwd()

student_performance = read.csv('student_performance.csv', header = TRUE, sep = ",")
student_performance

#2 Calculate mean and Standard deviation
#mean

mean_marks <- mean(student_performance$Final_Exam_Score)
mean_marks

#SD

sd_marks <- sd(student_performance$Final_Exam_Score)
sd_marks