# learning to use plotly

library(CyChecks2)
library(tidyverse)
library(plotly)
library(scales)


dat <- cyd_salprofs %>% 
  mutate(Total = total_salary_paid,
         Base = base_salary, 
         Travel = travel_subsistence) %>% 
  filter(prof_simp != "awarded prof")

#--make a smaller dataset to work with, just agronomy
dataset <- 
  dat %>%
      select(fiscal_year, dept, prof_simp, gender, id, Total, Base, Travel) %>%
      pivot_longer(Total:Travel) %>%
      filter(fiscal_year == 2018) %>%
      filter(dept == "agronomy") %>%
      mutate(value = round(value, 0))
  

#--control what is displayed when you hover your mouse
# source: https://www.musgraveanalytics.com/blog/2018/8/24/how-to-make-ggplot2-charts-interactive-with-plotly

dataset %>% 
  ggplot(aes(gender, value, group = 1,
             text = paste("Name:", id,
                          "<br>Salary: $", value))) +
  stat_summary(fun.y = mean, geom = "bar", aes(fill = gender)) + 
  geom_point() +
  facet_grid(name ~ prof_simp, scales = "free_y") +
  scale_y_continuous(labels = label_dollar()) +theme_bw() ->p1 # <-- comment this out for exploring

ggplotly(p1, tooltip = "text")

#--even better!

dataset %>% 
  ggplot(aes(gender, value, group = 1,
             text = paste("Name:", id, "(", gender, ")",
                          "<br>Salary: $", round(value/1000, digits = 0), "thou"))) +
  stat_summary(fun.y = mean, geom = "bar", aes(fill = gender)) + 
  geom_point() +
  facet_grid(name ~ prof_simp, scales = "free_y") +
  scale_y_continuous(labels = label_dollar()) +theme_bw() ->p1 # <-- comment this out for exploring

ggplotly(p1, tooltip = "text")

