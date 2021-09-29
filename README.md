# cha1
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week!
bike <- bike0 %>% 
  clean_names() %>% 
  dplyr::rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))

#my code here:
library(stringr)
library(ggplot2)

bike1 <- bike %>%
  filter(year>=2016 & year <=2019)  %>%
  group_by(year,month) %>%
  dplyr::summarize(monthly_mean_peryear=mean(bikes_hired))
  
bike2 <- bike1 %>%
  mutate(delta_rental_changes = monthly_mean_peryear - lag(monthly_mean_peryear,1))

bike2 %>%
ggplot(aes(x=month,y=delta_rental_changes))+
  geom_line()+
  scale_y_continuous(breaks=seq(0,40000))
  facet_wrap(~year,nrow=2)+
  theme_bw()
