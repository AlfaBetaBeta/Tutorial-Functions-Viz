# INDIVIDUAL WORK

#### (1) FUNCTIONS AND CONTROL STATEMENTS ####

# Create a function that manipulates the variables in a dataframe and returns the updated dataframe as follows:
# - if a variable is of class integer, coerce to numeric
# - if a variable is of class factor with more levels than given by a parameter (default 1000), coerce to character

# Version 1: for-loop embedding conditionals
# NOTE: && is a lazy operator
df_convert <- function (df, threshold=1000) {
    
    if (class(df) != 'data.frame') {stop('Argument <df> in df_convert must be a data.frame')}
    
    for (colname in colnames(df)) {
        
        if (class(df[,colname]) == 'integer') {
            df[,colname] <- as.numeric(df[,colname])
        }
        else if (class(df[,colname]) == 'factor' && length(levels(df[,colname])) > threshold) {
            df[,colname] <- as.character(df[,colname])
        }
    }
    return(df)
}

# Version 2: Vectorised functions with conditionals
df_convert2 <- function (df, threshold = 1000) {
    
    if (class(df) != 'data.frame') {stop('Argument <df> in df_convert2 must be a data.frame')}
    
    classes <- sapply(df, class)
    
    if ('integer' %in% classes) {
        df[classes == 'integer'] <- as.data.frame(lapply(df[classes == 'integer'],
                                                         as.numeric))
    }
    else if ('factor' %in% classes) {
        lvs <- sapply(df[classes == 'factor'], function(column) {length(levels(column))})
        
        if (any(lvs > threshold)) {
            df[names(lvs)[lvs>threshold]] <- as.data.frame(lapply(df[names(lvs)[lvs>threshold]],
                                                                  as.character),
                                                           stringsAsFactors = FALSE)
        }
    }
    return(df)
}

# Version 3: Library purrr, map functions and pipes with conditionals
library(purrr)
library(dplyr)
df_convert3 <- function (df, threshold = 1000) {
    
    if (class(df) != 'data.frame') {stop('Argument <df> in df_convert3 must be a data.frame')}
    
    classes <- map_chr(df, class)
    
    if ('integer' %in% classes) {
        columns <- subset(names(df), subset = classes == 'integer')
        df[,columns] <- df %>% select(columns) %>% map_df(as.numeric)
    }
    else if ('factor' %in% classes) {
        columns <- subset(names(df), subset = classes == 'factor')
        lvs <- df %>% select(columns) %>% map_dbl(~{length(levels(.x))})
        
        if (any(lvs > threshold)) {
            columns <- subset(names(lvs), subset = lvs > threshold)
            df[,columns] <- df %>% select(columns) %>% map_df(as.character)
        }
    }
    return(df)
}

# Version 4: Library data.table with conditionals (returns a datatable, though)
library(data.table)
df_convert4 <- function (df, threshold = 1000) {
    
    if (class(df) != 'data.frame') {stop('Argument <df> in df_convert4 must be a data.frame')}
    
    df <- as.data.table(df)
    classes <- sapply(df, class)
    
    if ('integer' %in% classes) {
        cols <- subset(names(df), subset = classes == 'integer')
        df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    }
    else if ('factor' %in% classes) {
        cols <- subset(names(df), subset = classes == 'factor')
        lvs <- df[, lapply(.SD, function(column) {length(levels(column))}), .SDcols = cols]
        
        if (any(lvs > threshold)) {
            cols <- subset(names(lvs), subset = lvs > threshold)
            df[, (cols) := lapply(.SD, as.character), .SDcols = cols]
        }
    }
    return(df)
}

# Read csv file into a dataframe and inspect its structure ####
# NOTE: csv file stored locally in the same directory as this R file
df_IN <- read.csv('./conditions.csv', stringsAsFactors = TRUE)
str(df_IN)
sapply(df_IN,class)

# Apply function version 1 with default threshold and inspect output 
df_OUT <- df_convert(df_IN)
str(df_OUT)
sapply(df_OUT,class)

# Apply function version 2 with default threshold and inspect output 
df_OUT2 <- df_convert2(df_IN)
str(df_OUT2)
sapply(df_OUT2,class)

# Apply function version 3 with default threshold and inspect output 
df_OUT3 <- df_convert3(df_IN)
str(df_OUT3)
sapply(df_OUT3,class)

# Apply function version 4 with default threshold and inspect output 
df_OUT4 <- df_convert4(df_IN)
str(df_OUT4)
sapply(df_OUT4,class)

# Apply function version 1 with threshold=100 and inspect output 
df_OUT <- df_convert(df_IN, 100)
str(df_OUT)
sapply(df_OUT,class)

# Apply function version 2 with threshold=100 and inspect output 
df_OUT2 <- df_convert2(df_IN, 100)
str(df_OUT2)
sapply(df_OUT2,class)

# Apply function version 3 with threshold=100 and inspect output 
df_OUT3 <- df_convert3(df_IN, 100)
str(df_OUT3)
sapply(df_OUT3,class)

# Apply function version 4 with threshold=100 and inspect output 
df_OUT4 <- df_convert4(df_IN, 100)
str(df_OUT4)
sapply(df_OUT4,class)


# Compare performance of all function versions on a dataframe with a large number of columns ####
library(lubridate)
dummy_df <- data.frame(c1 = as.factor(1:100),
                       c2 = 1:100)
for (i in 3:10000) {
    dummy_df <- cbind(dummy_df,dummy_df[[i%%2 + 1]])
    colnames(dummy_df)[i] <- paste0('c',i)
}
# Dummy dataframe of 10000 columns

time_start <- now()
df_out <- df_convert(dummy_df)
now() - time_start # Time difference of 4.68536 secs

time_start <- now()
df_out <- df_convert2(dummy_df)
now() - time_start # Time difference of 0.497952 secs

time_start <- now()
df_out <- df_convert3(dummy_df)
now() - time_start # Time difference of 1.190528 secs

time_start <- now()
df_out <- df_convert4(dummy_df)
now() - time_start # Time difference of 0.327656 secs

# The time difference values shown above are illustrative and depend on execution, but the general pattern
# is for version 4 to be the most efficient. The proper way to test this would be via a similar tool to
# the timeit library in python. It was not possible to include such study in this submission due to time
# constraints.


#### (2) VISUALIZATION ####

# Analize the age and gender distribution of patients with Hypertension versus patients with Diabetes
# (work only with patients that are alive today)
library(lubridate)
library(ggplot2)

# Set the default theme
theme_set(theme_minimal(base_size = 12))

# NOTE: csv files stored locally in the same directory as this R file
conditions <- read.csv('./conditions.csv', stringsAsFactors = TRUE)
str(conditions)
patients <- read.csv('./patients.csv', stringsAsFactors = TRUE)
str(patients)

# Merge (inner join) both dataframes on the patient code
df <- merge(x = conditions,
            y = patients,
            by.x = 'PATIENT',
            by.y = 'Id')
str(df)

# Subset df to retain only relevant info
df <- subset(df,
             subset = (DEATHDATE == '' & (DESCRIPTION == 'Diabetes' | DESCRIPTION == 'Hypertension')),
             select = c(BIRTHDATE, GENDER, DESCRIPTION))
# 'Reset' the DESCRIPTION factor levels to ('Diabetes','Hypertension') only
df$DESCRIPTION <- factor(df$DESCRIPTION, levels = c('Diabetes','Hypertension'))
str(df)

# Coerce column BIRTHDATE to Date class
df$BIRTHDATE <- as.Date(df$BIRTHDATE)
# Fix the present day as reference and transform date of birth to age
td <- today()
df$BIRTHDATE <- as.period(interval(df$BIRTHDATE, td))
str(df)
# Check the age range
max(df$BIRTHDATE$year) # 108
min(df$BIRTHDATE$year) # 18
# Bin the ages and store in a new factor column, ordering appropriately
df$age <- factor(ifelse(df$BIRTHDATE$year < 30, "18-30",
                        ifelse(df$BIRTHDATE$year < 40,"30-40",
                               ifelse(df$BIRTHDATE$year < 50,"40-50",
                                      ifelse(df$BIRTHDATE$year < 60,"50-60",
                                             ifelse(df$BIRTHDATE$year < 70,"60-70",
                                                    ifelse(df$BIRTHDATE$year < 80,"70-80",
                                                           ifelse(df$BIRTHDATE$year < 90,"80-90",">90"))))))),
                 levels = c('18-30','30-40','40-50','50-60','60-70','70-80','80-90','>90'))

# Visualisations via barplots
# Grid separating by gender and condition
p1 <- ggplot(data = df,
             aes(x = age, fill = GENDER,)) +
        geom_bar(stat='count') +
        ggtitle('Distribution of patients with Hypertension vs. Diabetes by gender and age group') +
        xlab('Age group [years]') +
        ylab('Frequency') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
              axis.title = element_text(face = 'bold'),
              strip.text.x = element_text(size = 10, face = 'italic'),
              strip.text.y = element_text(size = 10, face = 'italic', angle = 0)) +
        facet_grid(GENDER ~ DESCRIPTION)
# Save plot locally
ggsave('Facet_grid', plot = p1, device = 'png',
       width = 30, height = 20, units = 'cm')

# Facets by condition, stacking gender in the bars for ease of comparison
p2 <- ggplot(data = df,
             aes(x = age, fill = GENDER,)) + 
        geom_bar(stat='count') +
        ggtitle('Distribution of patients with Hypertension vs. Diabetes by gender and age group') +
        xlab('Age group [years]') +
        ylab('Frequency') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
              axis.title = element_text(face = 'bold'),
              strip.text.x = element_text(size = 12, face = 'italic')) +
        facet_wrap(~ DESCRIPTION)
# Save plot locally
ggsave('Facet_wrap_by_description', plot = p2, device = 'png',
       width = 30, height = 20, units = 'cm')

# Facets by gender, stacking condition in the bars for ease of comparison
p3 <- ggplot(data = df,
             aes(x = age, fill = DESCRIPTION,)) + 
        geom_bar(stat='count') +
        ggtitle('Distribution of patients with Hypertension vs. Diabetes by gender and age group') +
        xlab('Age group [years]') +
        ylab('Frequency') +
        theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
              axis.title = element_text(face = 'bold'),
              strip.text.x = element_text(size = 12, face = 'italic')) +
        facet_wrap(~ GENDER)
# Save plot locally
ggsave('Facet_wrap_by_gender', plot = p3, device = 'png',
       width = 30, height = 20, units = 'cm')

# COMMENTS
# For age groups up to 60-70, there are significantly more patients affected by hypertension than diabetes, regardless
# of gender.For older patients, the same trend holds but less accentuated.

# There are roughly three clusters of male patients by frequency:
# (1) 40-70
# (2) 18-40
# (3) >70

# There are also three clusters of female patients by frequency:
# (1) 18-40 & 50-60
# (2) 40-50 & 60-70
# (3) >70

# Diabetes is approximately equally distributed between genders in all age groups except the following
# 18-30
# 70-80
# >90
# where there is predominance of female patients.

# Hypertension is approximately equally distributed between genders in all age groups except the following
# 18-30
# 30-40
# 80-90
# where there is predominance of female patients in the first two and male predominance in the last age group.

# Based on the sample, the most vulnerable population cohort to diabetes are females aged 50-60 and males aged 40-50.
# Based on the sample, the most vulnerable population cohort to hypertension are females aged 18-30 and
# males aged 50-60.
