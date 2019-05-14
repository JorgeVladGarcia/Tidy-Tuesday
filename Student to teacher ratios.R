library(tidyverse)

# Getting the data 
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# Subsetting values for latam only
student_ratio<- data.frame(student_ratio)
latam<- c("MEX", "BRA", "CHIL", "PER", "ARG", "COL", "ECU", "BOL", "VEN", "HND")
student_ratio_latam<- student_ratio %>%
  filter(country_code %in% latam, !is.na(student_ratio))

# creating data frame with mean for LATAM
student_ratio_latam_mean<- 
student_ratio_latam %>%
  group_by(student_ratio_latam$year, indicator) %>%
  summarize(student_ratio_latam_mean = mean(student_ratio, na.rm = TRUE))
student_ratio_latam_mean$year<- student_ratio_latam_mean$`student_ratio_latam$year`

# Segmenting Bolivia dataset 
student_ratio_bol <- subset(student_ratio, country_code == "BOL")

# combining data sets
student_ratio_bol<- data.frame(student_ratio_bol)
student_ratio_latam_mean<- data_frame(student_ratio_latam_mean)

student_ratio_bol<- left_join(student_ratio_bol, student_ratio_latam_mean, by=c("indicator", "year"))

# drop columns that are not important
student_ratio_bol <- drop_columns(student_ratio_bol, c("flags", "flag_codes", "edulit_ind", "student_ratio_latam$year"))
student_ratio_bol <- na.omit(student_ratio_bol)

# to visualize frequency distributions for all discrete features
attach(student_ratio_bol)

ggplot(student_ratio_bol, aes_(fill=indicator, y=student_ratio_bol$student_ratio, x=year)) +
  geom_bar(position = "dodge",stat = "identity") +
  geom_line(aes(x=year, y=student_ratio_bol$student_ratio_latam_mean, stat="identity")) +
  facet_wrap(~indicator) +
  labs(title = "Alumnos por profesor en Bolivia",
       subtitle = "Promedio de alumnos por profesor en comparación con LATAM",
       x = "Año",
       y = "Proporción",
       fill = "Indicador",
       caption = "Fuente: UNESCO - #TidyTuesday
       Jorge García - @vladiroufakis") 