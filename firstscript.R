A <- 20

A

w = 45
e = 67.5
s = "Amal"
r = TRUE

class(w)
class(e)
class(s)
class(r)

#Vectors
#One Dimentional Arrays
vec_num1 = c(1:5)
vec_num2 = c(20,10,30,25,40)
vec_num3 = c(1:9)

vec_num4 = c(2,2,2)
vec_num5 = c(2,4,6)

vec_num1
vec_num2
vec_num3

#Vector Arithmetic
vec_num1+vec_num2
vec_num1-vec_num2
vec_num1*vec_num2
vec_num1/vec_num2
vec_num5^vec_num4

#Matrix
# 2 Dimension Arrays
z = c(1:6)
u = c(4,5,6,7,8,1)
mat_A = matrix(data=z, nrow = 3, ncol = 2, byrow = "False")

mat_A
mat_B = matrix(data = u, nrow = 3, ncol = 2, byrow = "True")

mat_B

# addition
add_AB = mat_A + mat_B
add_AB

# subtraction
sub_AB = mat_A - mat_B
sub_AB

# Division
dev_AB = mat_A / mat_B
dev_AB

# Multiplication
mul_AB = mat_A * mat_B
mul_AB

# Data Frames
a = c(10,20,30,40)
b = c(1:4)
c = c('M','I','S','O')

df_MY = data.frame(a,b,c)
names(df_MY) = c('ID','Values','Names')
df_MY

#PIE Chart
x = c(25,45,52,90)
y = c("A","B","C","D")

pie(x,y)

#advance Pie
pie(x,y, main = "Title of pie chart", col = rainbow(4))

#percentage cal
r = round(x/sum(x)*100)
r

new_label = paste(r,"%")
new_label

pie(x, new_label, main = "Title of the pie chart", col = rainbow(4))

#Bar charts
bar = c(25,34,76,90)

barplot(bar)

barplot(bar, main="Title of the bar chart")

barplot(bar, main="Title of the bar chart", col = "#16CE84")

barplot(bar, main="Title of the bar chart", col = "#16CE84",
        border = "#CE169E", names.arg = c("A","B","C","D"),
        xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart")

barplot(bar, main="Title of the bar chart", col = "#16CE84",
               border = "#CE169E", names.arg = c("A","B","C","D"),
               xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart",
               horiz = TRUE )

#Read_dataset
getwd()

Student_data = read.csv('stu.csv',header = TRUE, sep=",")
Student_data

#Central Tendency
#measyres of location
# 1) Mean (add all values and divide by how many values there are)
#ex 6,7,8
#Mean = (6+7+8)/3 = 7
mean(Student_data$Marks)

# 2) Median (put numbers in order and pick the middle one)
median(Student_data$Marks)

# 3) Mode(Most repeated value)
#Find the mode
mode_function <-function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

mode_function(Student_data$Marks)

#Measures of Dispersion(Range , Variance, Standard Deviation,
#IQR,Box plots)
#means how spread out the data is (Measure data variability)
getwd()

Student_data = read.csv('stu.csv',header = TRUE, sep=",")
Student_data

#Calculate Range
range_marks <- max(Student_data$Marks) - min(Student_data$Marks)
range_marks

#Calculate IQR Interquartile Range
iqr_marks <- IQR(Student_data$Marks)
iqr_marks

#Variance and Standard Deviation
var_marks <- var(Student_data$Marks)
var_marks

sd_marks <- sd(Student_data$Marks)
sd_marks





