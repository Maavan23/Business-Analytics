A <- 20

A
w = 45
e= 67.5
s= "Amal"
r= TRUE

class(w)
class(e)
class(s)
class(r)

#Vectors
#One_Dimensional_Array
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
mat_A = matrix(data=z, nrow = 3, ncol= 2, byrow = "False")

mat_A
mat_B = matrix(data=u, nrow = 3, ncol= 2, byrow = "True")

mat_B

#Addition
add_AB = mat_A+mat_B
add_AB

#Subtraction
sub_AB = mat_A-mat_B
sub_AB

#Division
dev_AB = mat_A/mat_B
dev_AB 

#Multiplication
mul_AB = mat_A*mat_B
mul_AB 

#Data Frames
a = c(10,20,30,40)
b = c(1,4)
c = c('M','I','S','O')

df_MY = data.frame(a,b,c)
names(df_MY) = c('ID','VALUES','NAMES')
df_MY

#PIE Chart
x=c(25,45,52,90)
y=c("A","B","C","D")

pie(x,y)

#advance Pie
pie(x,y, main = "title of the pie chart", col = rainbow(4))

#percentage cal
r = round(x/sum(x)*100)
r

new_label =paste(r,"%")
new_label

pie(x,new_label, main = "title of the pie chart", col = rainbow(4))

#Bar_Charts
bar = c(25,34,76,90)

barplot(bar)

barplot(bar, mai="Title of the bar chart")

barplot(bar, mai="Title of the bar chart",col ='#16CE84')

barplot(bar, mai="Title of the bar chart",col ='#16CE84',
        border ='#CE169E', names.arg = c("A","B","C","D"),
        xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart")

barplot(bar, mai="Title of the bar chart",col ='#16CE84',
        border ='#CE169E', names.arg = c("A","B","C","D"),
        xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart",
        horiz =TRUE )
                                         
        
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

#Calculate IQR Interquartile Range 
iqr_marks <- var(Student_data$Marks)
iqr_marks

#Variance and Standard Deviation
var_marks <- var(Student_data$Marks)
var_marks

sd_marks <- sd(Student_data$Marks)
sd_marks

#Box plot
boxplot(Student_data$Marks, main="Boxplot od Student Marks",
        ylab="Marks",
        col="Orange",
        border="Orange",
        notch=TRUE)

#Add mean line
abline(h=mean(Student_data$Marks), col = "red", lwd = 2, lty = 2)

#Add text label for mean
text(x = 1.3, y = mean(Student_data$Marks),
     labels = paste("Mean =", round(mean(Student_data$Marks))),
     col = "red")
     
#Get the dataset path
getwd()

Student_data = read.csv('stu.csv', header = TRUE, sep = ",")
Student_data

#ANOA TEST
result <- aov(Marks ~ Gender, data = Student_data)
summary(result)
#          Df Sum  Sq Mean Sq F value Pr(>F)
#Gender    1  144   144.1  0.429      0.514
#Residuals 98 32936 336.1
#if Pr(>F) < 0.o5 ==> Statically Significant
  #the average marks between Males and Females are Significantly different

#if Pr(>5) >= 0.05 ==> Not Statically insignificant
  #There is no strong evidence to say marks are different based on gender

#Frequency Distribution and Interval and midpoint
#1 create class intervals
breaks <- seq(0, 100, by = 10) #creates ex :- 0-10,10-20

#2 cut marks into Intervals
Student_data$Interval <- cut(Student_data$Marks, breaks = breaks, right = FALSE)

#3 Frequency table
freq_table <- table(Student_data$Interval)
freq_table

#Midpoint
midpoints <- (head(breaks, -1)) + tail(breaks, -1) /2
midpoints

#Create Histogram

barplot(freq_table,
        main = "Histogram of Marks",
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

Student_data = read.csv('stu.csv', header = TRUE, sep = ",")
Student_data

#2 Calculate mean and Standard deviation
#mean

mean_marks <- mean(Student_data$Marks)
mean_marks

#SD

sd_marks <- sd(Student_data$Marks)
sd_marks

# 3) create a sequence of X values
x_values <- seq(min(Student_data$Marks), max(Student_data$Marks), length=10)

#calculate the normal distribution Y values using dnorm()
y_values <- dnorm(x_values , mean = mean_marks, sd = sd_marks)

#4) plot
plot(x_values, y_values, type = "l", col = "blue", lwd =2, 
     main = "Bell curve of student marks",
     xlab = "Marks", ylab = "Density")

#with Package
install.packages("ggplot2")
library(ggplot2)

#Mean Standard Deviation
mean_marks <- mean(Student_data$Marks)
sd_marks <- sd(Student_data$Marks)

#Bell curve plot
ggplot(Student_data, aes(x = Marks)) +
  geom_histogram(aes(y=..density..), bins=15, fill="skyblue",
                 color="black", alpha=0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_marks, sd = sd_marks),
                color="red", size=1.2) +
  labs(title = "Bell Curve of Student Marks", x = "Marks", y = "Density")

#Hypothesis testing
#Perform a statistical hypothesis test to determine where there
#is a significant difference difference in the average marks obtained by 
#male and female students.
#Provide both numerical and graphical outputs.
#Clearly state the null and alternative hypotheses.
#Discuss the practical significance of your findings for 
#academic decision-making (eg:- gender-based performance support programs).
Student_data = read.csv('stu.csv', header = TRUE, sep = ",")
Student_data

#View Gender breakdown
table(Student_data$Gender)

#Check means 
aggregate(Marks ~ Gender, data = Student_data, mean)
#Visual comparison
boxplot(Marks ~ Gender, data = Student_data, col = c("lightblue", "lightgreen"),
        main = "Comparison of Marks by Gender",
        xlab = "Gender", ylab = "Marks")

#Perform two-sample t-test (independent samples)
t_test <- t.test(Marks ~ Gender, data = Student_data, var.equal = FALSE)

#Output
print(t_test)

#Interpretation
if (t_test$p.value < 0.05) {
  cat("Reject H₀ : There is statistically significant
      difference in average marks between male and female students.\n")
  
} else {
  cat("Do not reject H₀ : No significant difference
      in marks between male and female students.\n")
}

# ── auto_Info : Central Tendency ──────────────────────────────────────────────
# Dataset: 500 vehicles with columns engine_size, horsepower, curb_weight,
#          fuel_efficiency, age, price  (vehicle_type & brand are categorical)

# 1) Mean (add all values and divide by how many values there are)
#    ex: engine sizes 2.25, 1.93, 2.32  →  Mean = (2.25+1.93+2.32)/3 = 2.17
mean(auto_Info$engine_size)
mean(auto_Info$horsepower)
mean(auto_Info$curb_weight)
mean(auto_Info$fuel_efficiency)
mean(auto_Info$age)
mean(auto_Info$price)

# 2) Median (put numbers in order and pick the middle one)
#    With 500 values the median is the average of the 250th and 251st values
median(auto_Info$engine_size)
median(auto_Info$horsepower)
median(auto_Info$curb_weight)
median(auto_Info$fuel_efficiency)
median(auto_Info$age)
median(auto_Info$price)

# 3) Mode (most repeated value)
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_function(auto_Info$engine_size)
mode_function(auto_Info$horsepower)
mode_function(auto_Info$curb_weight)
mode_function(auto_Info$fuel_efficiency)
mode_function(auto_Info$age)
mode_function(auto_Info$price)
mode_function(auto_Info$vehicle_type)   # categorical – mode makes most sense here
mode_function(auto_Info$brand)          # categorical

# Calculate IQR (Interquartile Range)
# IQR = Q3 (75th percentile) - Q1 (25th percentile)
# ex: Q1 = 5, Q3 = 15  →  IQR = 15 - 5 = 10

iqr_engine_size    <- IQR(auto_Info$engine_size)
iqr_engine_size

iqr_horsepower     <- IQR(auto_Info$horsepower)
iqr_horsepower

iqr_curb_weight    <- IQR(auto_Info$curb_weight)
iqr_curb_weight

iqr_fuel_efficiency <- IQR(auto_Info$fuel_efficiency)
iqr_fuel_efficiency

iqr_age            <- IQR(auto_Info$age)
iqr_age

iqr_price          <- IQR(auto_Info$price)
iqr_price

# ── auto_Info : Variance, SD, Boxplot, ANOVA, Frequency, Bell Curve, Hypothesis ──

# Variance and Standard Deviation
var_engine_size     <- var(auto_Info$engine_size);     var_engine_size
var_horsepower      <- var(auto_Info$horsepower);      var_horsepower
var_curb_weight     <- var(auto_Info$curb_weight);     var_curb_weight
var_fuel_efficiency <- var(auto_Info$fuel_efficiency); var_fuel_efficiency
var_age             <- var(auto_Info$age);             var_age
var_price           <- var(auto_Info$price);           var_price

sd_engine_size      <- sd(auto_Info$engine_size);      sd_engine_size
sd_horsepower       <- sd(auto_Info$horsepower);       sd_horsepower
sd_curb_weight      <- sd(auto_Info$curb_weight);      sd_curb_weight
sd_fuel_efficiency  <- sd(auto_Info$fuel_efficiency);  sd_fuel_efficiency
sd_age              <- sd(auto_Info$age);              sd_age
sd_price            <- sd(auto_Info$price);            sd_price

# Box plot of Price
boxplot(auto_Info$price,
        main   = "Boxplot of Vehicle Price",
        ylab   = "Price ($)",
        col    = "lightblue",
        border = "darkblue",
        notch  = TRUE)

# Add mean line
abline(h = mean(auto_Info$price), col = "red", lwd = 2, lty = 2)

# Add text label for mean
text(x = 1.3, y = mean(auto_Info$price),
     labels = paste("Mean =", round(mean(auto_Info$price))),
     col = "red")

# ANOVA TEST — Does price differ across vehicle types?
result_auto <- aov(price ~ vehicle_type, data = auto_Info)
summary(result_auto)
# if Pr(>F) < 0.05 ==> Statistically Significant
#   Average price differs significantly across vehicle types
# if Pr(>F) >= 0.05 ==> Not Statistically Significant
#   No strong evidence that price differs by vehicle type

# Frequency Distribution — Intervals and Midpoints for Price
# 1) Create class intervals
breaks_price <- seq(0, 80000, by = 10000)  # 0-10000, 10000-20000, ...

# 2) Cut price into intervals
auto_Info$Interval <- cut(auto_Info$price, breaks = breaks_price, right = FALSE)

# 3) Frequency table
freq_table_price <- table(auto_Info$Interval)
freq_table_price

# Midpoints
midpoints_price <- (head(breaks_price, -1) + tail(breaks_price, -1)) / 2
midpoints_price

# Create Histogram
barplot(freq_table_price,
        main   = "Histogram of Vehicle Prices",
        xlab   = "Price Range ($)",
        ylab   = "Number of Vehicles",
        col    = "lightblue",
        border = "black")

# Frequency Polygon (Midpoints & Lines)
freq_values_price <- as.numeric(freq_table_price)

plot(midpoints_price, freq_values_price, type = "o",
     main = "Frequency Polygon of Vehicle Prices",
     xlab = "Midpoint of Price Range ($)",
     ylab = "Number of Vehicles",
     pch  = 16)

# Bell Curve
# 1) Calculate mean and Standard Deviation of price
mean_price <- mean(auto_Info$price)
mean_price

sd_price <- sd(auto_Info$price)
sd_price

# 2) Create a sequence of X values
x_values_price <- seq(min(auto_Info$price), max(auto_Info$price), length = 100)

# 3) Calculate the normal distribution Y values using dnorm()
y_values_price <- dnorm(x_values_price, mean = mean_price, sd = sd_price)

# 4) Plot
plot(x_values_price, y_values_price, type = "l", col = "blue", lwd = 2,
     main = "Bell Curve of Vehicle Prices",
     xlab = "Price ($)", ylab = "Density")

# With ggplot2
library(ggplot2)

ggplot(auto_Info, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue",
                 color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_price, sd = sd_price),
                color = "red", linewidth = 1.2) +
  labs(title = "Bell Curve of Vehicle Prices", x = "Price ($)", y = "Density")

# Hypothesis Testing
# Test whether there is a significant difference in average price
# between Toyota and Honda vehicles.
# H0 : Mean price of Toyota == Mean price of Honda
# H1 : Mean price of Toyota != Mean price of Honda

auto_two_brands <- subset(auto_Info, brand %in% c("Toyota", "Honda"))

# View brand breakdown
table(auto_two_brands$brand)

# Check means
aggregate(price ~ brand, data = auto_two_brands, mean)

# Visual comparison
boxplot(price ~ brand, data = auto_two_brands,
        col  = c("lightblue", "lightgreen"),
        main = "Comparison of Price by Brand",
        xlab = "Brand", ylab = "Price ($)")

# Perform two-sample t-test (independent samples)
t_test_auto <- t.test(price ~ brand, data = auto_two_brands, var.equal = FALSE)

# Output
print(t_test_auto)

# Interpretation
if (t_test_auto$p.value < 0.05) {
  cat("Reject H\u2080 : There is a statistically significant
      difference in average price between Toyota and Honda vehicles.\n")
} else {
  cat("Do not reject H\u2080 : No significant difference
      in price between Toyota and Honda vehicles.\n")
}


# ── auto_Info : Central Tendency ──────────────────────────────────────────────
# Dataset: 500 vehicles with columns engine_size, horsepower, curb_weight,
#          fuel_efficiency, age, price  (vehicle_type & brand are categorical)

# 1) Mean (add all values and divide by how many values there are)
#    ex: engine sizes 2.25, 1.93, 2.32  →  Mean = (2.25+1.93+2.32)/3 = 2.17
mean(auto_Info$engine_size)
mean(auto_Info$horsepower)
mean(auto_Info$curb_weight)
mean(auto_Info$fuel_efficiency)
mean(auto_Info$age)
mean(auto_Info$price)

# 2) Median (put numbers in order and pick the middle one)
#    With 500 values the median is the average of the 250th and 251st values
median(auto_Info$engine_size)
median(auto_Info$horsepower)
median(auto_Info$curb_weight)
median(auto_Info$fuel_efficiency)
median(auto_Info$age)
median(auto_Info$price)

# 3) Mode (most repeated value)
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_function(auto_Info$engine_size)
mode_function(auto_Info$horsepower)
mode_function(auto_Info$curb_weight)
mode_function(auto_Info$fuel_efficiency)
mode_function(auto_Info$age)
mode_function(auto_Info$price)
mode_function(auto_Info$vehicle_type)   # categorical – mode makes most sense here
mode_function(auto_Info$brand)          # categorical

# Calculate IQR (Interquartile Range)
# IQR = Q3 (75th percentile) - Q1 (25th percentile)
# ex: Q1 = 5, Q3 = 15  →  IQR = 15 - 5 = 10

iqr_engine_size    <- IQR(auto_Info$engine_size)
iqr_engine_size

iqr_horsepower     <- IQR(auto_Info$horsepower)
iqr_horsepower

iqr_curb_weight    <- IQR(auto_Info$curb_weight)
iqr_curb_weight

iqr_fuel_efficiency <- IQR(auto_Info$fuel_efficiency)
iqr_fuel_efficiency

iqr_age            <- IQR(auto_Info$age)
iqr_age

iqr_price          <- IQR(auto_Info$price)
iqr_price

# ── auto_Info : Variance, SD, Boxplot, ANOVA, Frequency, Bell Curve, Hypothesis ──

# Variance and Standard Deviation
var_engine_size     <- var(auto_Info$engine_size);     var_engine_size
var_horsepower      <- var(auto_Info$horsepower);      var_horsepower
var_curb_weight     <- var(auto_Info$curb_weight);     var_curb_weight
var_fuel_efficiency <- var(auto_Info$fuel_efficiency); var_fuel_efficiency
var_age             <- var(auto_Info$age);             var_age
var_price           <- var(auto_Info$price);           var_price

sd_engine_size      <- sd(auto_Info$engine_size);      sd_engine_size
sd_horsepower       <- sd(auto_Info$horsepower);       sd_horsepower
sd_curb_weight      <- sd(auto_Info$curb_weight);      sd_curb_weight
sd_fuel_efficiency  <- sd(auto_Info$fuel_efficiency);  sd_fuel_efficiency
sd_age              <- sd(auto_Info$age);              sd_age
sd_price            <- sd(auto_Info$price);            sd_price

# Box plot of Price
boxplot(auto_Info$price,
        main   = "Boxplot of Vehicle Price",
        ylab   = "Price ($)",
        col    = "lightblue",
        border = "darkblue",
        notch  = TRUE)

# Add mean line
abline(h = mean(auto_Info$price), col = "red", lwd = 2, lty = 2)

# Add text label for mean
text(x = 1.3, y = mean(auto_Info$price),
     labels = paste("Mean =", round(mean(auto_Info$price))),
     col = "red")

# ANOVA TEST — Does price differ across vehicle types?
result_auto <- aov(price ~ vehicle_type, data = auto_Info)
summary(result_auto)
# if Pr(>F) < 0.05 ==> Statistically Significant
#   Average price differs significantly across vehicle types
# if Pr(>F) >= 0.05 ==> Not Statistically Significant
#   No strong evidence that price differs by vehicle type

# Frequency Distribution — Intervals and Midpoints for Price
# 1) Create class intervals
breaks_price <- seq(0, 80000, by = 10000)  # 0-10000, 10000-20000, ...

# 2) Cut price into intervals
auto_Info$Interval <- cut(auto_Info$price, breaks = breaks_price, right = FALSE)

# 3) Frequency table
freq_table_price <- table(auto_Info$Interval)
freq_table_price

# Midpoints
midpoints_price <- (head(breaks_price, -1) + tail(breaks_price, -1)) / 2
midpoints_price

# Create Histogram
barplot(freq_table_price,
        main   = "Histogram of Vehicle Prices",
        xlab   = "Price Range ($)",
        ylab   = "Number of Vehicles",
        col    = "lightblue",
        border = "black")

# Frequency Polygon (Midpoints & Lines)
freq_values_price <- as.numeric(freq_table_price)

plot(midpoints_price, freq_values_price, type = "o",
     main = "Frequency Polygon of Vehicle Prices",
     xlab = "Midpoint of Price Range ($)",
     ylab = "Number of Vehicles",
     pch  = 16)

# Bell Curve
# 1) Calculate mean and Standard Deviation of price
mean_price <- mean(auto_Info$price)
mean_price

sd_price <- sd(auto_Info$price)
sd_price

# 2) Create a sequence of X values
x_values_price <- seq(min(auto_Info$price), max(auto_Info$price), length = 100)

# 3) Calculate the normal distribution Y values using dnorm()
y_values_price <- dnorm(x_values_price, mean = mean_price, sd = sd_price)

# 4) Plot
plot(x_values_price, y_values_price, type = "l", col = "blue", lwd = 2,
     main = "Bell Curve of Vehicle Prices",
     xlab = "Price ($)", ylab = "Density")

# With ggplot2
library(ggplot2)

ggplot(auto_Info, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue",
                 color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_price, sd = sd_price),
                color = "red", linewidth = 1.2) +
  labs(title = "Bell Curve of Vehicle Prices", x = "Price ($)", y = "Density")

# Hypothesis Testing
# Test whether there is a significant difference in average price
# between Toyota and Honda vehicles.
# H0 : Mean price of Toyota == Mean price of Honda
# H1 : Mean price of Toyota != Mean price of Honda

auto_two_brands <- subset(auto_Info, brand %in% c("Toyota", "Honda"))

# View brand breakdown
table(auto_two_brands$brand)

# Check means
aggregate(price ~ brand, data = auto_two_brands, mean)

# Visual comparison
boxplot(price ~ brand, data = auto_two_brands,
        col  = c("lightblue", "lightgreen"),
        main = "Comparison of Price by Brand",
        xlab = "Brand", ylab = "Price ($)")

# Perform two-sample t-test (independent samples)
t_test_auto <- t.test(price ~ brand, data = auto_two_brands, var.equal = FALSE)

# Output
print(t_test_auto)

# Interpretation
if (t_test_auto$p.value < 0.05) {
  cat("Reject H\u2080 : There is a statistically significant
      difference in average price between Toyota and Honda vehicles.\n")
} else {
  cat("Do not reject H\u2080 : No significant difference
      in price between Toyota and Honda vehicles.\n")
}

# ── auto_Info : Vehicle Age — Full Statistical Analysis ───────────────────────

# 1) Mean (add all values and divide by how many values there are)
#    ex: ages 5, 10, 7  →  Mean = (5+10+7)/3 = 7.33
mean(auto_Info$age)

# 2) Median (put numbers in order and pick the middle one)
#    With 500 values the median is the average of the 250th and 251st values
median(auto_Info$age)

# 3) Mode (most repeated value)
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_function(auto_Info$age)

# Calculate IQR (Interquartile Range)
# IQR = Q3 (75th percentile) - Q1 (25th percentile)
# ex: Q1 = 4, Q3 = 11  →  IQR = 11 - 4 = 7
iqr_age <- IQR(auto_Info$age)
iqr_age

# Variance and Standard Deviation
var_age <- var(auto_Info$age)
var_age

sd_age <- sd(auto_Info$age)
sd_age

# Box plot of Vehicle Age
boxplot(auto_Info$age,
        main   = "Boxplot of Vehicle Age",
        ylab   = "Age (Years)",
        col    = "lightgreen",
        border = "darkgreen",
        notch  = TRUE)

# Add mean line
abline(h = mean(auto_Info$age), col = "red", lwd = 2, lty = 2)

# Add text label for mean
text(x = 1.3, y = mean(auto_Info$age),
     labels = paste("Mean =", round(mean(auto_Info$age), 1)),
     col = "red")

# ANOVA TEST — Does vehicle age differ across vehicle types?
result_age <- aov(age ~ vehicle_type, data = auto_Info)
summary(result_age)
# if Pr(>F) < 0.05 ==> Statistically Significant
#   Average age differs significantly across vehicle types
# if Pr(>F) >= 0.05 ==> Not Statistically Significant
#   No strong evidence that age differs by vehicle type

# Frequency Distribution — Intervals and Midpoints for Age
# 1) Create class intervals (0-2, 2-4, 4-6, ... 12-14)
breaks_age <- seq(0, 14, by = 2)

# 2) Cut age into intervals
auto_Info$Age_Interval <- cut(auto_Info$age, breaks = breaks_age, right = FALSE)

# 3) Frequency table
freq_table_age <- table(auto_Info$Age_Interval)
freq_table_age

# Midpoints
midpoints_age <- (head(breaks_age, -1) + tail(breaks_age, -1)) / 2
midpoints_age

# Create Histogram
barplot(freq_table_age,
        main   = "Histogram of Vehicle Age",
        xlab   = "Age Range (Years)",
        ylab   = "Number of Vehicles",
        col    = "lightgreen",
        border = "black")

# Frequency Polygon (Midpoints & Lines)
freq_values_age <- as.numeric(freq_table_age)

plot(midpoints_age, freq_values_age, type = "o",
     main = "Frequency Polygon of Vehicle Age",
     xlab = "Midpoint of Age Range (Years)",
     ylab = "Number of Vehicles",
     pch  = 16)

# Bell Curve
# 1) Calculate mean and Standard Deviation of age
mean_age <- mean(auto_Info$age)
mean_age

sd_age <- sd(auto_Info$age)
sd_age

# 2) Create a sequence of X values
x_values_age <- seq(min(auto_Info$age), max(auto_Info$age), length = 100)

# 3) Calculate the normal distribution Y values using dnorm()
y_values_age <- dnorm(x_values_age, mean = mean_age, sd = sd_age)

# 4) Plot
plot(x_values_age, y_values_age, type = "l", col = "blue", lwd = 2,
     main = "Bell Curve of Vehicle Age",
     xlab = "Age (Years)", ylab = "Density")

# With ggplot2
library(ggplot2)

ggplot(auto_Info, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightgreen",
                 color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean_age, sd = sd_age),
                color = "red", linewidth = 1.2) +
  labs(title = "Bell Curve of Vehicle Age", x = "Age (Years)", y = "Density")

# Hypothesis Testing
# Test whether there is a significant difference in average age
# between Toyota and Honda vehicles.
# H0 : Mean age of Toyota == Mean age of Honda
# H1 : Mean age of Toyota != Mean age of Honda

auto_two_brands <- subset(auto_Info, brand %in% c("Toyota", "Honda"))

# View brand breakdown
table(auto_two_brands$brand)

# Check means
aggregate(age ~ brand, data = auto_two_brands, mean)

# Visual comparison
boxplot(age ~ brand, data = auto_two_brands,
        col  = c("lightblue", "lightgreen"),
        main = "Comparison of Vehicle Age by Brand",
        xlab = "Brand", ylab = "Age (Years)")

# Perform two-sample t-test (independent samples)
t_test_age <- t.test(age ~ brand, data = auto_two_brands, var.equal = FALSE)

# Output
print(t_test_age)

# Interpretation
if (t_test_age$p.value < 0.05) {
  cat("Reject H\u2080 : There is a statistically significant
      difference in average vehicle age between Toyota and Honda.\n")
} else {
  cat("Do not reject H\u2080 : No significant difference
      in vehicle age between Toyota and Honda.\n")
}

install.packages("modeest")
library(modeest)

#Covariance, Pearson correlation, and Spearman correlation all measure the relationship between two variables, but differ in what they measure and how they interpret the data. Covariance indicates the direction of a linear relationship and is on an unlimited scale. Pearson correlation measures the strength and direction of a linear relationship, with values between -1 and +1. Spearman correlation measures the strength and direction of a monotonic relationship by analyzing the ranks of the data, also with values between -1 and +1. 
#Covariance
#What it is: A measure of the joint variability of two random variables, showing their tendency to move together. 
#What it tells you: It indicates the direction of a relationship. A positive covariance means the variables tend to move in the same direction, while a negative covariance means they move in opposite directions. 
#Scale: Unlimited, ranging from -∞ to +∞. 

#Read_dataset
getwd()

Student_data = read.csv('stu.csv',header = TRUE, sep=",")
Student_data

#Normality testing
#1)Shapiro wilk testing
shapiro.test(Student_data$Marks)

#Shapiro wilk testing 
#p> 0.05 => Data is normally distributed
#p> 0.05 => Data is not Normally distributed

#2) Histogram for Normality Check
hist(Student_data$Marks,
     main = "Histogram of Marks",
     xlab = "Marks",
     col = "lightblue",
     border = "black")

#Histogram bell shaped => Likely normal 

#3QQ plot for normality check
qqnorm(Student_data$Marks,
       main = "Q-Q plot of Marks")
qqline(Student_data$Marks,
       col = "red",
       lwd = 2)
#Pointers on the straight line = likely normal


#Summary statistics for Age and Marks
summary(Student_data[,c("Age", "Marks")])

