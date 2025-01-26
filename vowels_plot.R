### An R script to get a csv file from Lin111 HW5 and plot vowels (adapted from class code)
### Files should be downloaded as CSV files
### The data should be formatted exactly like the HW5 template, with columns for:
### Vowel,Phrase,Time (s),F1 (Hz),F2 (Hz),Filename
library(tidyverse)
library(dplyr)
library(stringr)

# setwd("SET WORKING DIRECTORY HERE")

# df <- read.csv("REPLACE FILE NAME HERE")

### Set the column names of the dataframe
colnames(df) <- c("Vowel", "Phrase", "Time", "F1", "F2", "F3", "Filename")


### If needed, replace a with ɑ
df$Vowel <- str_replace_all(df$Vowel,"a", "ɑ")
df$Vowel <- str_replace_all(df$Vowel,"ə_a", "ə_ɑ")

### Create columns to indicate current vowel and full vowel in that phrase
df$Vowel1 <- str_sub(df$Vowel,0,1)
df$Vowel2 <- str_sub(df$Vowel,-1,-1)


### Factor the data
df$Vowel <- factor(df$Vowel, levels=c('ɑ', 'ə_ɑ', 'æ', 'ə_æ',
                                                    'i', 'ə_i', 'u', 'ə_u',
                                                    'ʌ','ə_ʌ'))
df$Vowel1 <- factor(df$Vowel1, levels=c('ɑ', 'æ', 'i', 'u', 'ʌ', 'ə'))
df$Vowel2 <- factor(df$Vowel2, levels=c('ɑ', 'æ', 'i', 'u', 'ʌ'))


### Create a dataframe with mean F1/F2 by vowel
df_means <- as.data.frame(df %>% group_by(Vowel,Vowel1) %>%
                          summarize(sum(F1)/n(), sum(F2)/n()))
colnames(df_means) <- c("Vowel","Vowel1", "F1", "F2")

df_means$Vowel <- factor(df_means$Vowel, levels=c('ɑ', 'ə_ɑ', 'æ', 'ə_æ',
                                              'i', 'ə_i', 'u', 'ə_u',
                                              'ʌ','ə_ʌ'))
df_means$Vowel1 <- factor(df_means$Vowel1, levels=c('ɑ', 'æ', 'i', 'u', 'ʌ', 'ə'))




### Generate a plot of all data points
ggplot(df, aes(x=F2, y=F1, color=Vowel, fill=Vowel)) +
  scale_color_manual(values=c('#FF0000','#FF0000', '#FFAA00','#FFAA00',
                              '#0000FF','#0000FF','#00FF00', '#00FF00',
                              '#AA00FF','#AA00FF','#000000'))+ 
  geom_text(label=df$Vowel1, size=6)+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()



### Generate a plot of mean data points
ggplot(df_means, aes(x=F2, y=F1, color=Vowel)) +
  scale_color_manual(values=c('#FF0000','#FF0000', '#FFAA00','#FFAA00',
                              '#0000FF','#0000FF','#00FF00', '#00FF00',
                              '#AA00FF','#AA00FF','#000000'))+ 
  geom_text(label=df_means$Vowel1, size=8)+
  scale_x_reverse()+
  scale_y_reverse()+theme_bw()





#### Save image files of the plot to your computer
### Generate a plot of all data points
plot_all <- ggplot(df, aes(x=F2, y=F1, color=Vowel, fill=Vowel)) +
  scale_color_manual(values=c('#FF0000','#FF0000', '#FFAA00','#FFAA00',
                              '#0000FF','#0000FF','#00FF00', '#00FF00',
                              '#AA00FF','#AA00FF','#000000'))+ 
  geom_text(label=df$Vowel1, size=6)+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()



### Generate a plot of mean data points
plot_means <- ggplot(df_means, aes(x=F2, y=F1, color=Vowel)) +
  scale_color_manual(values=c('#FF0000','#FF0000', '#FFAA00','#FFAA00',
                              '#0000FF','#0000FF','#00FF00', '#00FF00',
                              '#AA00FF','#AA00FF','#000000'))+ 
  geom_text(label=df_means$Vowel1, size=8)+
  scale_x_reverse()+
  scale_y_reverse()+theme_bw()



ggsave("plot-all.jpg", plot_all, width=18, height = 10, units = 'cm', dpi = 300)
ggsave("plot-means.jpg", plot_means, width=18, height = 10, units = 'cm', dpi = 300)
