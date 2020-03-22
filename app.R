library(ggplot2)
library(CyChecks2)
library(tidyverse)
library(shinythemes) #--not working
library(plotly)
library(scales) #--to get $ on y axis, so easy!

dat <- cyd_salprofs %>% 
    mutate(Total = total_salary_paid,
           Base = base_salary, 
           Travel = travel_subsistence) %>% 
    filter(prof_simp != "awarded prof")

dd_year <- dat %>% select(fiscal_year) %>% pull() %>% unique() %>% sort()
dd_dept <- dat %>% select(dept) %>% pull() %>% unique() %>% sort()
#dd_st <- c("Total", "Base", "Travel")


ui <- fluidPage(title = "Salary Explorer",
                theme = shinytheme("sandstone"), # <-- why isn't this working??
                
                plotlyOutput('plot'),
                
                hr(),
                
                fluidRow(column(2,
                                h4("Something")
                                # selectizeInput(inputId = 'myst', 
                                #                label = 'Salary Type', 
                                #                choices = dd_st, 
                                #                selected = "Base",
                                #                )
                                ),
                         column(
                             5,
                             selectizeInput('myy', 'Year', dd_year, "2018")
                             ),
                         column(
                             5,
                             selectizeInput('myd', 'Department', dd_dept, "agronomy")
                             )
                         )
                )
                

server <- function(input, output) {

    dataset <- reactive({

        dat %>%
            select(fiscal_year, dept, prof_simp, gender, id, Total, Base, Travel) %>%
            pivot_longer(Total:Travel) %>%
            filter(fiscal_year == input$myy) %>%
            filter(dept == input$myd) %>%
            mutate(value = round(value, 0))


    })

    output$plot <- renderPlotly({

        
        
        p1 <- dataset() %>% 
            ggplot(aes(gender, value, group = 1,
                       text = paste("Name:", id, "(", gender, ")",
                                    "<br>Salary: $", round(value/1000, digits = 0), "thou"))) +
            stat_summary(fun.y = mean, geom = "bar", aes(fill = gender)) + 
            geom_point() +
            facet_grid(name ~ prof_simp, scales = "free_y") +
            scale_y_continuous(labels = label_dollar())
        
        ggplotly(p1, tooltip = "text")
        

    })
}

shinyApp(ui, server)