# Libraries
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(readr)
library(stringr)

divorce <- read_csv("marriage/divorce.csv")[, -c(1,3, 7, 16)]


divorce_long_education <- divorce %>%
  pivot_longer(c(2:6, 10:14), names_to = "education", values_to = "divorced_pop") %>%
  mutate(divorce_rate = divorced_pop * 100, 
         education_level = case_when(
           education == "all_3544" ~ "Overall",
           education == "HS_3544" ~ "High school or less",
           education == "SC_3544" ~ "Some college",
           education == "BAo_3544" ~ "College graduate",
           education == "GD_3544" ~ "Graduate degree",
           education == "all_4554" ~ "Overall",
           education == "HS_4554" ~ "High school or less",
           education == "SC_4554" ~ "Some college",
           education == "BAo_4554" ~ "College graduate",
           education == "GD_4554" ~ "Graduate degree"),
         age_range = case_when(
           str_detect(education, "3544") ~ "35-44",
           str_detect(education, "4554") ~ "45-54")
  )
divorce_education <- divorce_long_education %>%
  mutate(education_level = fct_relevel(divorce_long_education$education_level,
                                       "Overall",
                                       "Graduate degree",
                                       "College graduate",
                                       "Some college",
                                       "High school or less")) %>%
  select(year, divorce_rate, education_level, age_range)

divorce_long_wealth <- divorce %>%
  pivot_longer(c(2, 7:10, 15:17), names_to = "wealth", values_to = "divorced_pop") %>%
  mutate(divorce_rate = divorced_pop * 100, 
         wealth_level = case_when(
           wealth == "all_3544" ~ "Overall",
           wealth == "poor_3544" ~ "Family income in lowest 25%",
           wealth == "mid_3544" ~ "Family income in middle 50%",
           wealth == "rich_3544" ~ "Family income in highest 25%",
           wealth == "all_4554" ~ "Overall",
           wealth == "poor_4554" ~ "Family income in lowest 25%",
           wealth == "mid_4554" ~ "Family income in middle 50%",
           wealth == "rich_4554" ~ "Family income in highest 25%"
         ),
         age_range = case_when(
           str_detect(wealth, "3544") ~ "35-44",
           str_detect(wealth, "4554") ~ "45-54")
  )
divorce_wealth <- divorce_long_wealth %>%
  mutate(wealth_level = fct_relevel(divorce_long_wealth$wealth_level,
                                    "Overall",
                                    "Family income in highest 25%",
                                    "Family income in middle 50%",
                                    "Family income in lowest 25%")) %>%
  select(year, divorce_rate, wealth_level, age_range)


# User interface
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Investing Divorce Rates (1960-2012)"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("education_group", label = h3("Education Level"), 
                         choices = list("Overall" = "Overall",
                                        "Graduate degree" = "Graduate degree",
                                        "College graduate" = "College graduate",
                                        "Some college" = "Some college",
                                        "High school or less" = "High school or less"
                         ),
                         selected = "Overall"),
      
      #make a group of checkboxes [WEALTH]
      checkboxGroupInput("wealth_group", label = h3("Wealth Level"), 
                         choices = list("Overall" = "Overall",
                                        "Family income in highest 25%" = "Family income in highest 25%",
                                        "Family income in middle 50%" = "Family income in middle 50%",
                                        "Family income in lowest 25%" = "Family income in lowest 25%"
                         ),
                         selected = "Overall"),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      
      #make a group of checkboxes [AGERANGE]
      checkboxGroupInput("age_group", label = h3("Age Range"), 
                         choices = list("35-44" = "35-44",
                                        "45-54" = "45-54"),
                         selected = c("35-44", "45-54")),
      
      
      
      
      # Create slider widget
      sliderInput("year_range", "Range of Years:",
                  min = min(divorce_education$year), 
                  max = max(divorce_education$year),
                  value = c(1960,2012),
                  sep = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", tags$p("Marriage rates among Americans have been on the decline for several decades, 
                                 while divorce rates have increased. The difference in divorce rates between groups, such as class or education, 
                                 can give insight to what factors impact the likelihood of a marriage ending in divorce and also provides 
                                 information on the family structure of these groups. For which groups has the divorce rate increased the most?"),
                 plotOutput(outputId = "graph_education"),
                 plotOutput(outputId = "graph_wealth")),
        tabPanel("Data", tags$p("Data from the FiveThirtyEight ", 
                                tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/marriage",
                                       "Marriage dataset.")),
                 tags$p("The Pew Research Center documented the marriage and divorce rates of Americans since 1960. 
                        The specific data used for this app was the ",
                        tags$code("divorce.csv"), " file. 
                        The data explores the divorce rates across educational levels as well as class levels."))
      )
    )
  )
)
# Server function
server <- function(input, output, session){
  education <- reactive({ 
    divorce_education %>%
      filter(education_level %in% input$education_group,
             year >= input$year_range[1], 
             year <= input$year_range[2],
             age_range %in% input$age_group) 
  })
  wealth <- reactive({ 
    divorce_wealth %>%
      filter(wealth_level %in% input$wealth_group,
             year >= input$year_range[1], 
             year <= input$year_range[2],
             age_range %in% input$age_group) 
  })
  output$graph_education <- renderPlot({
    ggplot(data = education(), 
           mapping = aes(x = year, y = divorce_rate,
                         color = education_level, 
                         linetype = age_range)) +
      geom_line() +
      labs(y = "Divorce Rate",
           x = "Year",
           color = "Education Level",
           linetype = "Age Range")
  })
  output$graph_wealth <- renderPlot({
    ggplot(data = wealth(), 
           mapping = aes(x = year, y = divorce_rate,
                         color = wealth_level, 
                         linetype = age_range)) +
      geom_line() +
      labs(y = "Divorce Rate",
           x = "Year",
           color = "Wealth Level",
           linetype = "Age Range")
  })
  # dat_names_agg <- reactive({ 
  #   dat_names() %>%
  #     group_by(name) %>%
  #     summarize(count = sum(n)) %>%
  #     arrange(desc(count))
  # })
  # output$table <-  renderDataTable({
  #   datatable(dat_names_agg(), 
  #             options = list(paging = FALSE,
  #                            searching = FALSE,
  #                            orderClasses = TRUE))
  # })
}

# Creates app
shinyApp(ui = ui, server = server)