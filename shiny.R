
library(shiny)
library(forecast)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)

covid <- read.csv("COVID-19_Cases_and_Deaths_by_Age_Group_-_ARCHIVE.csv")
drugs <- read.csv("Accidental_Drug_Related_Deaths_2012-2022.csv")


covid$Date <- as.Date(covid$Date, format = "%m/%d/%Y")
drugs$Date <- as.Date(drugs$Date, format = "%m/%d/%Y")

only_ints<- covid %>%
  group_by(Date) %>%
  summarise(across(everything(), ~ if(is.numeric(.)) sum(.) else first(.)))

drugs$Date <- as.Date(drugs$Date, format = "%m/%d/%Y")

joined_no_dupes <- left_join(drugs, only_ints, by = c("Date"))
covid_combined <- covid %>%
  group_by(DateUpdated) %>%
  summarise(across(all_of("Total.deaths"), ~ sum(.x, na.rm = TRUE)))

covid_combined <- covid_combined %>%
  mutate(date = as.Date(DateUpdated, format = "%m/%d/%Y")) %>%
  filter(year(date) > 2019 & date > as.Date("2020-01-01"))
drugs2020 <- drugs %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(year(Date) > 2019 & Date > as.Date("2020-01-01"))

date_counts <- drugs2020 %>%
  group_by(Date) %>%
  summarise(count = n())
cumulative_drug_deaths <- date_counts %>%
  mutate(cumulative_deaths = cumsum(count))
merged_data <- covid_combined %>%
  left_join(cumulative_drug_deaths, by = c("date" = "Date")) %>%
  left_join(mutate(drugs2020, date = Date), by = c("date" = "date"))

forecast_plot_data_2020 <- NULL
ui <- fluidPage(
  titlePanel("Drug Overdose Deaths vs Date: Post Covid Starting"),
  tabsetPanel(
    type = "tabs",
    tabPanel("Overview",
             fluidRow(
               column(width = 12,
                      h3("Project Overview"),
                      p("My project analyzes the correlation between COVID-19 deaths and drug overdose deaths."),
                      hr(),
                      h4("Data Sources"),
                      p("I have two main data sources, the first being a dataset a COVID-19 dataset that incldues a running total of deaths and other statistics between 2020 and 2022 in Connecticut. The other dataset is a drug overdose dataset, which includes all drug overdoses and their features in Connecticut between 2012 and 2022."),
                      hr(),
                      h4("Analysis"),
                      p("What I have included in this Shiny app are snapshots of both the datasets I have mentioned, four interactive models that give an overview of some features of each dataset, and a forecasting model that comapres how many drug overdose deaths would have happened had COVID never happened."),
                      hr()
               )
             )
    ),
    tabPanel("Visualizations",
             tabsetPanel(
               tabPanel(
                 "Heroin Overdoses by Age",
                 fluidRow(
                   column(width = 8, plotOutput("heroin_plot")),  
                   column(width = 4,
                          sliderInput("buckets", "Number of Buckets",
                                      min = 1, max = 20, value = 5, step = 1)  
                   )
                 )
               ),
               tabPanel(
                 "Drug Presence",
                 fluidRow(
                   column(width = 6,
                          plotOutput("drug_pie")),
                   column(width = 6,
                          sliderInput("threshold", "Threshold for Drug Counts", 
                                      min = 0, max = 5000, value = 1000, step = 100),
                          sliderInput("year_range", "Year Range", 
                                      min = 2012, max = 2022, value = c(2012, 2022), step = 1)
                   )
                 )
               ),
               tabPanel(
                 "County Distribution",  
                 fluidRow(
                   column(width = 6,
                          plotOutput("county_distribution")),  
                   column(width = 6,
                          sliderInput("threshold_county", "Threshold for County Counts",
                                      min = 0, max = 1000, value = 300, step = 50),
                          sliderInput("year_range_county", "Year Range for Counties", 
                                      min = 2012, max = 2022, value = c(2012, 2022), step = 1)
                   )
                 )
               ),
               tabPanel(
                 "Relationship Between COVID Deaths and Drug Overdoses",
                 fluidRow(
                   column(width = 12,
                          plotOutput("covid_drug_relationship"),
                          sliderInput("line_parts", "Split Graph into Parts:",
                                      min = 1, max = 10, value = 5, step = 1)
                   )
                 )
               )
             )
             
    ),
    tabPanel("Tabulations",
             tabsetPanel(
               tabPanel("COVID Data",
                        h2("COVID Dataset"),
                        DTOutput("covid_table")
               ),
               tabPanel("Drugs Data",
                        h2("Drugs Dataset"),
                        DTOutput("drugs_table")
               )
             )
    ),
    tabPanel(
      "Models",
      fluidRow(
        column(width = 6, plotOutput("model_plot")),  
        column(width = 6, verbatimTextOutput("model_summary"))
      )
    )
  )
)



model_ts <- NULL

server <- function(input, output) {
  

  output$model_plot <- renderPlot({
    covid <- read.csv("COVID-19_Cases_and_Deaths_by_Age_Group_-_ARCHIVE.csv")
    drugs <- read.csv("Accidental_Drug_Related_Deaths_2012-2022.csv")
    covid$Date <- as.Date(covid$Date, format = "%m/%d/%Y")
    drugs$Date <- as.Date(drugs$Date, format = "%m/%d/%Y")
    date_counts <- drugs %>%
      group_by(Date) %>%
      summarise(count = n())
    
    cumulative_drug_deaths <- date_counts %>%
      mutate(cumulative_deaths = cumsum(count)) %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
    
    training_data <- cumulative_drug_deaths %>%
      filter(Date < as.Date("2020-04-05"))
    testing_data <- cumulative_drug_deaths %>%
      filter(Date >= as.Date("2020-04-05"))
    
    ts_training <- ts(training_data$cumulative_deaths, frequency = 365)
    
    model_ts <<- forecast::auto.arima(ts_training)
    
    forecast_values <- forecast(model_ts, h = nrow(testing_data))$mean
    
    forecast_plot_data <- data.frame(
      Date = c(training_data$Date, testing_data$Date),
      Cumulative_Deaths = c(training_data$cumulative_deaths, testing_data$cumulative_deaths),
      Forecasted_Deaths = c(rep(NA, nrow(training_data)), forecast_values)
    )
    
    forecast_plot_data_2020 <<- forecast_plot_data %>%
      filter(Date >= as.Date("2020-04-05"))
    
    ggplot(forecast_plot_data_2020, aes(x = Date)) +
      geom_line(aes(y = Cumulative_Deaths, color = "True")) +
      geom_line(aes(y = Forecasted_Deaths, color = "Forecasted"), linetype = "dashed") +
      geom_ribbon(aes(ymin = pmin(Cumulative_Deaths, Forecasted_Deaths), ymax = pmax(Cumulative_Deaths, Forecasted_Deaths)), fill = "grey") +
      labs(title = "Drug Overdose Deaths vs Date: Post Covid Starting",
           x = "Date",
           y = "Cumulative Drug Deaths") +
      scale_color_manual(values = c("True" = "blue", "Forecasted" = "red")) 
  })
  
  output$model_summary <- renderPrint({
    differences <- forecast_plot_data_2020$Forecasted_Deaths - forecast_plot_data_2020$Cumulative_Deaths
    
    t_test_result <- t.test(differences, mu = 0)
    
    print(t_test_result)
    print(summary(model_ts))
  })
  
  output$heroin_plot <- renderPlot({
    summary <- joined_no_dupes %>%
      filter(Heroin == "Y") %>%
      group_by(Age = cut(Age, input$buckets)) %>%
      summarize(Count = n())
    
    ggplot(summary, aes(x = Age, y = Count)) +
      geom_bar(stat = "identity", fill = "blue", width = 0.5) +
      labs(
        title = "Heroin Overdoses by Age",
        x = "Age",
        y = "Overdoses"
      )
  })
  output$drug_pie <- renderPlot({
    selected_columns <- c("Heroin", "Cocaine", "Fentanyl", "Fentanyl.Analogue", "Oxycodone", "Oxymorphone", "Ethanol", "Hydrocodone", "Benzodiazepine", "Methadone", "Meth.Amphetamine", "Amphet", "Tramad", "Hydromorphone", "Morphine..Not.Heroin." ,"Xylazine", "Gabapentin", "Opiate.NOS","Heroin.Morph.Codeine", "Other.Opioid", "Any.Opioid")
    
    selected_data <- drugs[selected_columns]
    dates <- as.Date(drugs$Date, format = "%m/%d/%Y")
    selected_data <- selected_data[dates >= as.Date(paste(input$year_range[1], "-01-01", sep = "")) & 
                                     dates <= as.Date(paste(input$year_range[2], "-12-31", sep = "")), ]
    
    y_counts <- sapply(selected_data, function(col) sum(tolower(col) %in% c("y", "Y")))
    
    drug_counts <- data.frame(Drug = names(y_counts), Count = y_counts)
    
    drug_counts <- drug_counts[drug_counts$Count > 0, ]
    
    drug_counts$Drug[drug_counts$Count < input$threshold] <- "Other"
    drug_counts <- aggregate(Count ~ Drug, drug_counts, sum)
    
    drug_counts <- drug_counts[order(drug_counts$Count, decreasing = TRUE), ]
    
    
    pie(drug_counts$Count, labels = drug_counts$Drug, main = "Presence of Drugs in Overdose Deaths 2012-2022")
    
  })
  output$county_distribution <- renderPlot({

    drugs$Death.County[drugs$Death.County == ""] <- "Unknown"
    

    selected_data <- drugs[
      year(drugs$Date) >= input$year_range_county[1] &
        year(drugs$Date) <= input$year_range_county[2], ]
    

    counties <- table(selected_data$Death.County)
    

    threshold <- input$threshold_county
    count <- sum(counties[counties < threshold])
    group <- ifelse(counties >= threshold, counties, 0)
    

    if (count > 0) {
      group <- append(group, count)
      names(group)[length(group)] <- "Other"
    }
    

    pie(group, labels = ifelse(group > 0, names(group), ""), main = "Overdose Death County Distribution")
  })
  
  output$covid_drug_relationship <- renderPlot({

    num_lines <- input$line_parts
    
    gg <- ggplot(data = merged_data, aes(x = Total.deaths, y = cumulative_deaths, color = Age)) +
      geom_point() +
      scale_color_gradient(low = "black", high = "yellow") +
      labs(title = "Relationship Between COVID Deaths, Age, and Drug Overdose Deaths",
           x = "Cumulative COVID Deaths", y = "Cumulative Drug Deaths") +
      coord_cartesian(ylim = c(0, max(merged_data$cumulative_deaths) * 1.05))
    

    for (i in seq(0, 1, length.out = num_lines + 2)[-c(1, num_lines + 2)]) {
      gg <- gg + geom_vline(xintercept = quantile(merged_data$Total.deaths, probs = i), linetype = "dashed", color = "red")
    }
    
    print(gg)
  })
  
  output$covid_table <- renderDT({
    datatable(covid, filter = "top", 
              options = list(pageLength = 10, dom = 't', autoWidth = TRUE))
  })
  
  output$drugs_table <- renderDT({
    datatable(drugs, filter = "top", 
              options = list(pageLength = 10, dom = 't', autoWidth = TRUE))
  })

  
}

shinyApp(ui = ui, server = server)