# This R file contains the source code needed to run the examples 
# described in a paper "R you ready? Using the R programme for 
# statistical analysis and graphics"


# We should let R know which packages we are planning to use
# At the sametim, we are also checking if they are installed
list.of.packages <- c("furniture", "officer", "flextable", "magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(furniture)
library(officer)
library(flextable)
library(magrittr)

# Load the data from a csv file
data <- read.csv("db_TEIQue.csv", header = T)
# Print some information on the data we just read to check 
# whether the data loaded properly
str(data)

# Separate TEIQue-SF values in a variable named teiq
teiq <- data[,4:33]

# Recode the varables that need to be reversed
teiq[,c(16,2,18,4,5,7,22,8,10,25,26,12,13,28,14)] <- 8-teiq[,c(16,2,18,4,5,7,22,8,10,25,26,12,13,28,14)]

# Calculate four factor scores
data$wb <- rowSums(teiq[,c(5,20,9,24,12,27)])/6
data$sc <- rowSums(teiq[,c(4,19,7,22,15,30)])/6
data$emo <- rowSums(teiq[,c(1,16,2,17,8,23,13,28)])/8
data$soc <- rowSums(teiq[,c(6,21,10,25,11,26)])/6

# Calculate total TEIQue-SF score
data$totalTEIQue <- rowSums(teiq)/30

# Prepare data and create summary statistics table
data$Gender <- as.factor(data$Gender)
levels(data$Gender) <-c("Female", "Male")

furniture::table1(data, "Gender" = Gender, "Well being" = wb, "Self control" = sc, 
                  "Emotionality"=emo, "Sociability"=soc,
                  splitby = ~PreviousCaringExperience,
                  test = TRUE,
                  na.rm = FALSE
) -> tab1

# Print summary table here
tab1

# Prepare correlation plot
# Items 3, 18, 14 & 29 can be omitted from the analysis as 
# Petrides (2006) considered them a ‘general’ factor and 
# were not specifically associated with any particular factor.
teiq <- teiq[,-c(3, 18, 14, 29)]
corrplot(cor(teiq), order = "original", tl.col='black', tl.cex=.75)

# Create an image containing the plot
png(file = "figure1.png", units="in", width=6, height=6, res=300)
corrplot(cor(teiq), order = "original", tl.col='black', tl.cex=.75)
dev.off()

# Create word document and export summary table
read_docx() %>% 
  body_add_par(value = "Table 1. Summary statistics for TEIQue-SF 30", style = "table title") %>% 
  body_add_flextable(flextable(tab1[[1]]) %>% autofit(), align = "left") %>%
  body_add_img(src = "figure1.png", width = 6, height = 6) %>%
  body_add_par(value = "Figure 1. Correlation plot for TEIQue-SF 30", style = "graphic title") %>% 
  print(target = "Summary table.docx")
