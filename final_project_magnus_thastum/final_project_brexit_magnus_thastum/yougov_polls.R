## Plotting YouGov polles
library(here)
library(tidyverse)

poll_fr <- read_csv(here("data","data_france.csv"))
poll_uk <- read_csv(here("data","data_uk.csv"))


## UK plots
uk_leave <- poll_uk[["To leave the EU"]]
uk_leave <- gsub("%","", as.character(uk_leave))
uk_leave <- as.numeric(uk_leave)
mean(uk_leave) #41.65517
median(uk_leave) #41

uk_remain <- poll_uk[["To remain a member of the EU"]]
uk_remain <- gsub("%","", as.character(uk_remain))
uk_remain <- as.numeric(uk_remain)
mean(uk_remain) #40.29885
median(uk_remain) #41

uk_date <- poll_uk[["Fieldwork End Date / Pollster"]]
uk_date <- gsub(" / YouGov","", as.character(uk_date))

uk_year <- str_extract(uk_date, "20..")
uk_year <- as.numeric(uk_year) 
uk_year

Uk_plot <- data.frame(uk_year, uk_leave, uk_remain)

uk_p1 <- Uk_plot %>% 
  ggplot(aes(x = uk_year, y = uk_leave)) +
  geom_point() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE)
uk_p1

uk_p2 <- Uk_plot %>% 
  ggplot(aes(x = uk_year, y = uk_remain)) +
  geom_point() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE)
uk_p2

uk_plot_remain_mean <- aggregate(Uk_plot$uk_remain, list(Uk_plot$uk_year), FUN=mean)

uk_p3 <- uk_plot_remain_mean %>% 
  ggplot(aes(x = Group.1, y = x)) +
  geom_line() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Remain (%)") +
  ggtitle("Remain Poll UK")
uk_p3

uk_plot_leave_mean <- aggregate(Uk_plot$uk_leave, list(Uk_plot$uk_year), FUN=mean)

uk_p4 <- uk_plot_leave_mean %>% 
  ggplot(aes(x = Group.1, y = x)) +
  geom_line() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Leave (%)") +
  ggtitle("Leave Poll UK")
uk_p4

## France plots
fr_leave <- poll_fr[["Leave the EU"]]
fr_leave <- gsub("%","", as.character(fr_leave))
fr_leave <- as.numeric(fr_leave)
mean(fr_leave) #29.60606
median(fr_leave) #30

fr_remain <- poll_fr[["Remain in the EU"]]
fr_remain <- gsub("%","", as.character(fr_remain))
fr_remain <- as.numeric(fr_remain)
mean(fr_remain) #45.9697
median(fr_remain) #47

fr_date <- poll_fr[["Fieldwork End Date / Pollster"]]
fr_date <- gsub(" / YouGov","", as.character(fr_date))
fr_date <- gsub(" /","", as.character(fr_date))

fr_year <- str_extract(fr_date, "20..")
fr_year <- as.numeric(fr_year) 
fr_year

fr_plot <- data.frame(fr_year, fr_leave, fr_remain)

fr_p1 <- fr_plot %>% 
  ggplot(aes(x = fr_year, y = fr_leave)) +
  geom_point() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Leave (%)") +
  ggtitle("Leave Poll France")
fr_p1

fr_p2 <- fr_plot %>% 
  ggplot(aes(x = fr_year, y = fr_remain)) +
  geom_point() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Remain (%)") +
  ggtitle("Remain Poll France")
fr_p2

fr_plot_remain_mean <- aggregate(fr_plot$fr_remain, list(fr_plot$fr_year), FUN=mean)

fr_p3 <- fr_plot_remain_mean %>% 
  ggplot(aes(x = Group.1, y = x)) +
  geom_line() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Remain (%)") +
  ggtitle("Remain Poll France")
fr_p3

fr_plot_leave_mean <- aggregate(fr_plot$fr_leave, list(fr_plot$fr_year), FUN=mean)

fr_p4 <- fr_plot_leave_mean %>% 
  ggplot(aes(x = Group.1, y = x)) +
  geom_line() +
  stat_smooth(method = "lm", aes(color = "linear"), se = FALSE) +
  xlab("Year") +
  ylab("Leave (%)") +
  ggtitle("Leave Poll France")
fr_p4

