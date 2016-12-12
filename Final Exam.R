#######################################################################
###################### Question Two ###################################

data<-matrix(c(8,10,15,7,6,8,6,12,6,20,7,2,10,6,6,28),ncol = 4) # Drinks
colnames(data) <- c("Psychology","Physics","Philosophy","Other") # Column Names
rownames(data) <- c("Soft drink","Beer","Wine","Other") # Row Names

# If required instal "gplots"
install.packages("gplots")

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(data))
# 2. Graph
balloonplot(t(dt), main ="data", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
chisq <- chisq.test(data)
chisq
# Observed counts
chisq$observed
# Expected counts
round(chisq$expected,2)
#  residuals
round(chisq$residuals, 3)
# Visualization of residuals
#For a given cell, the size of the circle is proportional to the amount of the cell contribution.
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
# printing the p-value
chisq$p.value
# printing the mean
chisq$estimate


######################################################################
################## Question Three ####################################


Midterm_One = c(20,20,15,16,20) # Midterm One
Midterm_Two= c(28,19,21,25,22) # Midterm Two

t.test(Midterm_One, Midterm_Two)

# Data in two numeric vectors
# ++++++++++++++++++++++++++
# Test scores before studying with a group
Midterm_One <- c(20,20,15,16,20)
# Test scores after studying with a group
Midterm_Two <-c(28,19,21,25,22)
# Create a data frame
my_data1 <- data.frame( 
  group = rep(c("Midterm_One", "Midterm_Two"), each = 5),
  Results = c(Midterm_One,  Midterm_Two)
)
# Print all data
print(my_data1)

library("dplyr")
group_by(my_data1, group) %>%
  summarise(
    count = n(),
    median = median(Results, na.rm = TRUE),
    IQR = IQR(Results, na.rm = TRUE)
  )
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data1, x = "group", y = "Results", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("Midterm_One", "Midterm_Two")
       )
res <- wilcox.test(Midterm_One, Midterm_Two, paired = TRUE)
res
# print only the p-value
res$p.value
