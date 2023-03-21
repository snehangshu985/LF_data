#===============================================================================
# Analysis of Ladle Furnace Data 
#===============================================================================

# loading the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
# seting the wroking directory
setwd("F:/LF_data")
#-----------------------------------------------------------------------------
# loading and cleaning of data set
#-----------------------------------------------------------------------------
# loading the data set

df <- read_excel("data/aprildata.xlsx")
# converting the data set from tibble to data frame
df <- as.data.frame(df)
# selectig the columns LHF & from 2 to 12
df <- df %>% select(LHF, 2:12)

# only taking the complete data set
df <- subset(df, complete.cases(df))
# changing the variable names
mynames <- c("LHF", "Heat_No", "LADLE_IN", "ARCING_START", "ARCING_END", 
             "INITIAL_TEMP", "FINAL_TEMP","Total_arcing_time", 
             "C", "Si", "Mn", "S")

mynames <- tolower(mynames)

names(df) <- mynames

# taking the only the part of the data set in which EDA will be done
df <- df %>% select("lhf", "heat_no", "initial_temp", "final_temp", 
                    "total_arcing_time", "c", "si", "mn", "s")

# recoding the factor lhf 2 and 1
df$lhf <- recode(df$lhf,  "LHF 2" = "lhf2", "LHF 1" = "lhf1" )

# taking the only the minutes of arching
df$total_arcing_time <- minute(df$total_arcing_time)


df_mod <- df%>% mutate (temp_increase = final_temp - initial_temp)
df_mod <- df_mod %>% select(-heat_no, - initial_temp, -final_temp)

my_id <- length(df_mod$si)

for(i in 1:my_id) {
    if(df_mod$si[i] > 1){
        df_mod$si[i] = df_mod$si[i]/100
    }
}

for(i in 1:my_id) {
    if(df_mod$mn[i] > 3){
        df_mod$mn[i] = df_mod$mn[i]/10
    }
}

for(i in 1:my_id) {
    if(df_mod$mn[i] > 3){
        df_mod$mn[i] = df_mod$mn[i]/10
    }
}


#-----------------------------------------------------------------------------
# Exploratory Data Analysis
#-----------------------------------------------------------------------------

# make basic scatter plot of arcing time vs temperature increase
sct_t_a <- ggplot(df_mod, aes(total_arcing_time, 
                              temp_increase, color = lhf)) +
    geom_point(position = "jitter") + 
    geom_smooth(method = "lm", se=F) + 
    ylab("Increase in Temperature") + 
    xlab("Total Arcing Time") +
    labs(color = "LHF")

# histogram
h_arctime <- ggplot(df_mod, aes(total_arcing_time)) + 
    geom_histogram(bins = 10) + 
    facet_grid(lhf ~ .)

h_temp_increase <- ggplot(df_mod, aes(temp_increase)) + 
    geom_histogram(bins = 10, fill="light blue", color = "red") + 
    facet_grid(lhf ~ .) 
    
    
# box_plot

df_mod2 <- df_mod %>% gather("c", "si",  
                             key = "Elements", value = "Chemistry")


b_plot <- ggplot(df_mod2, aes(Elements, Chemistry)) + 
    geom_point(color = "blue", alpha = .3, position = "jitter") +
    geom_boxplot()


ggp <- ggplot(df_mod2, aes(x = Chemistry, 
                    color = Elements, fill = Elements)) + 
    geom_density(alpha = .3)
















