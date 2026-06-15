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