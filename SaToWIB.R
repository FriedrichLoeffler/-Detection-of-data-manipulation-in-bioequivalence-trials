# Imitation of the SaToWIB routine
################################################################################
### 1. Repository ###

# Please create a folder on your local computer where you save the provided example
# files and where the program will store the results.
# To see the results, you will need to enter the path to your folder in three places in this code:
# chapter 3, chapter 4 and chapter 7.
# The folder path starts with (" and ends with ". The wording behind the last slash does not need to be changed. It is the name and type of the file.
# Example:
##########################
### ENTER FOLDER PATH: ###
# tb1 <- read_excel("C:/Users/Friedrich Loeffler/OneDrive/Desktop/git project/formatted PK data.xlsx", col_types = "text")

# IMPORTANT: When pasting the path you have to change all SLASHs to BACKSLASHs!!!


################################################################################
### 2. Import libraries in R ###
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

################################################################################
### 3. Import and organizing data set ###

##########################
### ENTER FOLDER PATH: ###
tb1 <- read_excel("C:/Users/kuern/OneDrive/Desktop/git project/formatted PK data.xlsx", col_types = "text") # import Excel-file.


tb1 <- type.convert(tb1, as.is = TRUE)                                    # converts the values in the tibble to numeric <dbl> ot integer <int> type.
tb1 <- unite(tb1, subject, period, treatment, col = "Subject", sep = "|") # combines values of the column "subject", "period" and "treatment" to ONE column with the name "Subject".
                                                                          # the values of the three columns are seperated by "|".
tb1 <- tb1[,-2]                                                           # delete column 2 as not needed.
tb1[,2] <- round(tb1[,2],1)                                               # round the time point of taking the sample to make the time point uniform.
tb <- pivot_wider(tb1, names_from = time, values_from = concentration)    # change the dataframe (tibble) to "wide arrangement".
tb <- tb[,-(28:29)]                                                       # due to the exact data collection of the time point there are deviations here that we delete.
tb <- fill(tb,9, .direction = "down")                                     # fill the NA values with the previous value of that column.
tb <- fill(tb,12, .direction = "down")
tb2 <- tb[, -1]                                                           # create separated dataframe without first column.

tb.subj <- pivot_longer(tb,  cols = 2:27, names_to = "Time_hours", values_to = "Concentration ng/mL") # change the dataframe (tibble) to "long arrangement".
tb.subj$`Concentration ng/mL` <- format(round(tb.subj$`Concentration ng/mL`, 2), nsmall = 2)          # round the "concentration" to 2 decimals.

tb <- type.convert(tb, as.is = TRUE)                                      # final conversion
tb2 <- type.convert(tb2, as.is = TRUE)
tb.subj <- type.convert(tb.subj, as.is = TRUE)

################################################################################
### 4. SIMILARITY SCORE ###

data_tbl <- as.data.frame(tb)
df <- as.data.frame(tb2)

# define variables
a <- 1
j <- 0
i <- 1
x <- j+1
y <- i+1
n <- (nrow(data_tbl)-1)                                                   # number of rows
row_x <- slice(df,x)                                                      # first row of data sheet
row_y <- slice(df,y)                                                      # compared row of data sheet
sbn_x <- data_tbl[x,1]                                                    # Subject name x
sbn_y <- data_tbl[y,1]                                                    # Subject name y

# function according to Fuglsang paper
calculate_score_xy <- function(row_x, row_y){
  sum(abs(row_x - row_y)/((row_x + row_y)*0.5))/length(row_x)             # formula to calculate the similarity score between plasma samples of two subjects
}

cal <- calculate_score_xy(row_x, row_y)

# define the result data frame
result <- data.frame()

# outer loop
for (j in 1:n) {
  k <- j
  # inner loop
  for (i in k:n) {
    y <- i+1
    row_x <- slice(df,x)                                                  # first row of data sheet
    row_y <- slice(df,y)                                                  # compared row of data sheet
    sbn_x <- data_tbl[x,1]                                                # Subject name x row
    sbn_y <- data_tbl[y,1]                                                # Subject name y row
    
    cal <- calculate_score_xy(row_x, row_y)                               # calculates the similarity score for two subjects
    # cat(sbn_x, sbn_y, calculate_score_xy(row_x, row_y), "\n")           # prints results
    output <- c(sbn_x, sbn_y, cal)                                        # output is one row of the data frame merging subject names and score together with vector function.
    result <- rbind(result, output)                                       # rbind() merges each created row of the loop to the dataframe "result"
  }
  x <- j+1
}
output

# formating the result and rounding the score
result1 <- type.convert(result, as.is = TRUE)                             # convert result to numeric datatypes
colnames(result1) <- c("Profile 1", "Profile 2", "Score")                 # column names added to dataframe with vector function
result1$Score <- format(round(result1$Score,6), nsmall = 6)               # format only the "Score" column to only 6 decimals. This automatically formats all columns back to characters
#options(max.print = 100000)                                              # enlarges the number of printings

result2 <- type.convert(result1, as.is = TRUE)                            # convert result to numeric datatypes
result3 <- result2[order(result2$Score),]                                 # sort column "Score" in ascending order
#print(result3)

##########################
### ENTER FOLDER PATH: ###
write.csv2(result3,"C:/Users/kuern/OneDrive/Desktop/git project/Similarity_plasma_results.csv", row.names = FALSE)

################################################################################
#### 5. Create dataframe "pairs" with most similar subject-pair-IDs in one column #

pairs <- data.frame()
for (k in 1:4) {
  paira <- result3[k,1]
  pairs <- rbind(pairs, paira)
  pairb <- result3[k,2]
  pairs <- rbind(pairs, pairb)
}
names(pairs) <- c("Subject")
print(pairs)                                                              # print the 4 closest subject pairs in the console

################################################################################
### 6. PAINT GRAPHS WITH THE 4 MOST SIMILAR SUBJECT VALUES ###
library(patchwork)
library(ggrepel)

tb.subj <- as.data.frame(tb.subj)                                          # change to dataframe as tibble does not work
tb.subj <- type.convert(tb.subj, as.is = TRUE)
tb.subj1 <- filter(tb.subj, Subject == pairs[1,1] | Subject == pairs[2,1]) # filter the data of the first pair
tb.subj2 <- filter(tb.subj, Subject == pairs[3,1] | Subject == pairs[4,1]) # filter the data of the second pair
tb.subj3 <- filter(tb.subj, Subject == pairs[5,1] | Subject == pairs[6,1]) # filter the data of the third pair
tb.subj4 <- filter(tb.subj, Subject == pairs[7,1] | Subject == pairs[8,1]) # filter the data of the fourth pair

rows <- nrow(result3)*0.5                                                  # calculates the median pair of result3
mpaira <- result3[rows, 1]
mpairb <- result3[rows, 2]
tb.subj5 <- filter(tb.subj, Subject == mpaira | Subject == mpairb)         # filter the data of the median pair

### create the four plots p1, p2, p3, p4 ###

p1 <- ggplot(tb.subj1, aes(x = tb.subj1[, 2], y = tb.subj1[, 3], group = Subject, colour = Subject, shape = Subject)) +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  labs(x = "Time in hours",
       y = "Concentration in ng. per m.L.",
       subtitle = "The closest plasma concentration results",
  ) + scale_x_log10()

p2 <- ggplot(tb.subj2, aes(x = tb.subj2[, 2], y = tb.subj2[, 3], group = Subject, colour = Subject, shape = Subject)) +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  labs(x = "Time in hours",
       y = "Concentration in ng. per m.L.",
       subtitle = "The 2nd closest plasma concentration results",
  ) + scale_x_log10()

p3 <- ggplot(tb.subj3, aes(x = tb.subj3[, 2], y = tb.subj3[, 3], group = Subject, colour = Subject, shape = Subject)) +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  labs(x = "Time in hours",
       y = "Concentration in ng. per m.L.",
       subtitle = "The 3rd closest plasma concentration results",
  ) + scale_x_log10()

p4 <- ggplot(tb.subj4, aes(x = tb.subj4[, 2], y = tb.subj4[, 3], group = Subject, colour = Subject, shape = Subject)) +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  labs(x = "Time in hours",
       y = "Concentration in ng. per m.L.",
       subtitle = "The 4th closest plasma concentration results",
  ) + scale_x_log10()

p5 <- ggplot(tb.subj5, aes(x = tb.subj5[, 2], y = tb.subj5[, 3], group = Subject, colour = Subject, shape = Subject)) +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  labs(x = "Time in hours",
       y = "Concentration in ng. per m.L.",
       subtitle = "The Median plasma concentration result",
  ) + scale_x_log10()

################################################################################
### 7. EXPORT PLOTS TO pdf ###

##########################
### ENTER FOLDER PATH: ###
pdf(file = "C:/Users/kuern/OneDrive/Desktop/git project/Similarity plasma sample plots.pdf", paper = "a4", height = 11, onefile = TRUE)

### calling the four plots and set up the layout of the page
p1/p2/p3/p4/p5 +
  geom_point(size = 2.0) +
  geom_line(linewidth = 0.3) +
  theme_light() +
  scale_x_log10() +
  plot_annotation(
    title = "Blood plasma concentration of samples with the closest values",
    caption = "Illustration according to Fuglsang A., 2021. Detection of data manipulation in bioequivalence trials. EJPS, 156 (2021) 105595 realized by Friedrich Loeffler") &
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 7, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )
dev.off()

