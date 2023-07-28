library(tidyverse)
library(ggplot2)
library(agricolae)

model_q1_Pre <- aov(Pre_total_grade ~ class, data = Data)

model_q1_Pre_lsd <- LSD.test(model_q1_Pre , "class")

print(model_q1_Pre_lsd)
ggplot(Data, aes(class %>% ordered(model_q1_Pre_lsd$groups %>% row.names()), 
                 y = Pre_total_grade)) + 
  stat_boxplot(alpha=0.2) + 
  xlab("class") +
  ylab("Previous total grade") +
  stat_summary(geom = 'text', 
               label = model_q1_Pre_lsd$groups[,2], 
               fun.y = max, size = 4, 
               vjust=0, hjust =-1, color ="darkblue")
