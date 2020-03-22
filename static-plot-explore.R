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


# try on bi-plot ----------------------------------------------------------

#--n values
dat %>% 
  select(fiscal_year, gender, id, prof_simp, dept, base_salary) %>% 
  group_by(fiscal_year, dept) %>% 
  mutate(n = n()) %>% 
  arrange(dept, fiscal_year, prof_simp)

#-m pct?
neq <- dat %>% 
  select(fiscal_year, gender, id, prof_simp, dept, base_salary) %>% 
  group_by(fiscal_year, dept, gender, prof_simp) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
    mutate(M = replace_na(M, 0),
           `F` = replace_na(`F`, 0)) %>% 
  mutate(ntot = M + `F`,
         mn_pct = M/ntot) %>%
  select(-M, -`F`)


#-m/f sal ratio
saleq <- dat %>% 
  select(fiscal_year, gender, prof_simp, dept, base_salary) %>%
  group_by(fiscal_year, prof_simp, dept, gender) %>% 
  summarise(base_salary = mean(base_salary, na.rm = T)) %>% 
  pivot_wider(names_from = gender, values_from = base_salary) %>% 
  mutate(mfsal_pct = (M/`F`)) %>% 
  select(-`F`, -M)

library(ggpubr)
dat2 <- 
  neq %>% 
  left_join(saleq) %>% 
  mutate(mfsal_pct = case_when(   #--if the dept is only one geneder, assign it a mfsal of 100 (perfectly equal)
    mn_pct == 0 ~ 1,
    mn_pct == 100 ~1,
    TRUE ~ mfsal_pct)) 
  
  ggplot(dat2, aes(mfsal_pct, mn_pct,
             group = 1,
             text = paste("Dept:", dept,
                          "<br>Faculty:", round(mn_pct*100, digits = 0), "% male",
                          "<br>Male Salary is", round(mfsal_pct*100, digits = 0), "% of Female"))) +
    geom_hline(yintercept = 0.5) + 
    geom_vline(xintercept = 1) +
    geom_point(aes(size = ntot)) + 
  geom_rug(color = "red") +
    guides(size = F, color = F) +
      facet_grid(prof_simp~.) +
    theme_pubclean() + 
    theme(panel.grid = element_blank()) + 
  scale_y_continuous(labels = label_percent()) +
    scale_x_continuous(labels = label_percent()) 
    
    theme_bw() #->p1 # <-- comment this out for exploring

ggplotly(p1, tooltip = "text")

