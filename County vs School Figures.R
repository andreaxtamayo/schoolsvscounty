#install.packages("tidyverse")
library(tidyverse)
library(lubridate)

all_counties <- read_csv("./Data/allCPSandPS.csv", show_col_types = FALSE)
all_counties$NewDate <- ymd(paste(all_counties$Year, all_counties$Month, all_counties$Date, sep = "-"))

plot_county_and_school = function(all_counties, county_name, county_col, school_col){
  file_name = paste0(county_name, ".png")
  png(file_name, width = 2100, height = 400, res = 120)
  print(
    ggplot() +
      geom_line(data = all_counties, mapping = aes(x = all_counties$NewDate, y = all_counties[[county_col]]/max(all_counties[[county_col]]), color = "County")) +
      geom_line(data = all_counties, mapping = aes(x = all_counties$NewDate, y = all_counties[[school_col]]/max(all_counties[[school_col]]), color = "Public Schools")) +
      labs(title = paste(county_name, "County vs School"), x = "Date", y = "Reported Cases") +
      theme_bw() +
      theme(text=element_text(size = 16)) +
      scale_color_manual(values = c("County" = "blue", "Public Schools" = "red"))
  )
  dev.off()
}
  
plot_county_and_school(all_counties, 'Alachua', 'AC New Reported Cases', 'AVGACPS New Reported Cases')

plot_county_and_school(all_counties, "Polk", 'PolC Daily Average', 'AVGPolCPS')

plot_county_and_school(all_counties, "Duval", 'DC', 'AVGDuvCPS')

plot_county_and_school(all_counties, "Broward", 'BroCPS', 'Bro C Weekly Average')

plot_county_and_school(all_counties, "Orange", 'OC Daily Average', 'AVGOCPS')