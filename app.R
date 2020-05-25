library(shiny)
library(ggplot2)
library(CyChecks2)
library(tidyverse)
library(shinythemes) #--not working
library(plotly)
library(scales) #--to get $ on y axis, so easy!
library(ggpubr) # for theme_pubclean



# data --------------------------------------------------------------------

awardprof <- c("distg prof", "univ prof", "morrill prof")

#--data for salary fig
dat <- 
    professors %>% 
    mutate(Total = total_salary_paid,
           Base = base_salary, 
           Travel = travel_subsistence,
           dept = str_to_title(DEPT_SHORT_NAME)) 


dat_sals <- 
    dat %>%
    filter(!grepl("chair|adj|affil|emer|vstg|chr|clin|collab|res", title)) %>%
    mutate(prof_simp = ifelse(title %in% awardprof, "Awarded Prof", title),
           prof_simp = str_to_title(prof_simp),
           prof_simp = factor(prof_simp, levels = c("Asst Prof", "Assoc Prof", "Prof", "Awarded Prof")))


dat_sals %>% 
    select(prof_simp) %>% 
    distinct()


#-m pct
dat_ns <- 
    dat_sals %>% 
    select(year, gender, prof_simp, dept, base_salary) %>% 
    group_by(year, dept, gender, prof_simp) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = gender, values_from = n) %>% 
    mutate(M = replace_na(M, 0),
           `F` = replace_na(`F`, 0)) %>% 
    mutate(ntot = M + `F`,
           mn_pct = M/ntot) %>%
    select(-M, -`F`)


#-m/f sal ratio
dat_srat <- 
    dat_sals %>% 
    select(year, gender, prof_simp, dept, base_salary) %>%
    group_by(year, prof_simp, dept, gender) %>% 
    summarise(base_salary = mean(base_salary, na.rm = T)) %>% 
    pivot_wider(names_from = gender, values_from = base_salary) %>% 
    mutate(mfsal_pct = (`F`/M)) %>% 
    select(-`F`, -M) 


dat2 <- 
    dat_ns %>% 
    left_join(dat_srat) %>% 
    mutate(mfsal_pct = case_when(   #--if the dept is only one geneder, assign it a mfsal of 100 (perfectly equal)
        mn_pct == 0 ~ 1,
        mn_pct == 100 ~1,
        is.na(mfsal_pct) ~ 1,
        TRUE ~ mfsal_pct))


dd_year <- dat %>% select(year) %>% pull() %>% unique() %>% sort()
dd_dept <- dat %>% select(dept) %>% pull() %>% unique() %>% sort()
#dd_st <- c("Total", "Base", "Travel")





ui <-
    fluidPage(theme = shinytheme("sandstone"),
              
              navbarPage(
                  "Iowa State Salary Data",
                
                  tabPanel("Overview",
                           plotlyOutput('plot2'),
                           
                           hr(),
                           
                           fluidRow(
                               column(6,
                                      selectizeInput('myy2', 'Year', dd_year, "2018")),
                               column(6,
                                      selectizeInput('myd2', 'Department to Highlight', dd_dept, "Agronomy"))
                               )
                           ),
                  
                  tabPanel(
                      "Salaries",
                      plotlyOutput('plot1'),
                      
                      # hr(),
                      
                      fluidRow(
                          column(12,
                                 h4("Hover over a point\nto see more info")),
                          br(),
                          column(6,
                                 selectizeInput('myy1', 'Year', dd_year, "2018")),
                          column(6,
                                 selectizeInput('myd1', 'Department', dd_dept, "Agronomy"))
                      )
                  )
                  )
              )

server <- function(input, output) {

    dataset1 <- reactive({

        dat %>%
            select(year, dept, prof_simp, gender, id, Total, Base, Travel) %>%
            pivot_longer(Total:Travel) %>%
            filter(year == input$myy1) %>%
            filter(dept == input$myd1) %>%
            mutate(value = round(value, 0))


    })

    output$plot1 <- renderPlotly({

        
        
        p1 <- dataset1() %>% 
            ggplot(aes(gender, value, group = 1,
                       text = paste("Name:", id, "(", gender, ")",
                                    "<br>Salary: $", round(value/1000, digits = 0), "thou"))) +
            stat_summary(fun.y = mean, geom = "bar", aes(fill = gender)) + 
            geom_point() +
            guides(fill = F) +
            facet_grid(name ~ prof_simp, scales = "free_y") +
            scale_y_continuous(labels = label_dollar())
        
        ggplotly(p1, tooltip = "text")
        

    })
    
    
    
    dataset2 <- reactive({
        
        dat2 %>%
            filter(year == input$myy2) %>%
            mutate(mycolor = ifelse(dept == input$myd2, "Y", "N"))
    })
    
    output$plot2 <- renderPlotly({
        
        
        
        p2 <- dataset2() %>% 
            ggplot(aes(mfsal_pct, mn_pct,
                       group = 1,
                       text = paste("Dept:", dept,
                                    "<br>Total Faculty:", ntot, ";", round(mn_pct*100, digits = 0), "% male",
                                    "<br>Female Salary is", round(mfsal_pct*100, digits = 0), "% of Male"))) +
            geom_hline(yintercept = 0.5, linetype = "dotted") + 
            geom_vline(xintercept = 1, linetype = "dotted") +
            geom_point(aes(size = ntot, color = mycolor, alpha = mycolor)) + 
            geom_rug(color = "orange") +
            guides(size = F, color = F) +
            scale_color_manual(values = c("N" = "black", 
                                          "Y" = "orange")) +
            scale_alpha_manual(values = c("N" = 0.5,
                                          "Y" = 1)) +
            facet_grid(.~prof_simp, scales = "free") +
            labs(x = "\nPercentage of Male Salary Earned by Females\n",
                 y = "\nPercentage of Faculty Who Are Male\n") +
            #theme_pubclean() + 
            theme(panel.grid = element_blank(),
                  axis.title = element_text(margin = margin(t = 20))) + 
            scale_y_continuous(labels = label_percent()) +
            scale_x_continuous(labels = label_percent(),  breaks = c(0.5, 1, 1.5, 2, 2.5, 3)) 
        
        ggplotly(p2, tooltip = "text")
        
        
    })
    
}

shinyApp(ui, server)