# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(ggridges)
library(plotly)

# Load the dataset
trips_hh <- read.csv("data/trips_household.csv")

# Create a UI with navbar layout
ui <- navbarPage(
  "Travel Analysis",   # The title of the navbar
  
  # Add custom CSS for navbar colors and font sizes
  tags$head(
    tags$style(HTML("
      .navbar { 
        background-color: #003366;
      }
      .navbar-default .navbar-nav > li > a, 
      .navbar-default .navbar-brand {
        color: white;
        font-size: 18px;  /* Increase font size in navbar */
      }
      .navbar-default .navbar-nav > .active > a {
        background-color: #002244;
        color: white;
      }
      h3 { font-size: 22px; }  /* Increase font size for headers */
      h4 { font-size: 20px; }  /* Increase font size for subheaders */
      p { font-size: 18px; }   /* Increase font size for paragraph text */
      .control-label { font-size: 18px; }  /* Increase font size for input labels */
    "))
  ),
  # Create a tab called "Overview" 
  tabPanel("Overview",
           fluidPage(
             titlePanel("Travel Mode by Time Period"),
             p("Explore the distribution of each travel pattern across different time periods on weekdays and weekends.
               You can follow the instructions to explore the data and play with the plot yourselves."),
             
             fluidRow(
               column(9, 
                      plotlyOutput("interactive_bar_chart", height = "100vh")
               ),
               
               column(3, 
                      h3("Filters"),
                      checkboxGroupInput("mode_filter", "Groupings to plot:", 
                                         choices = c("Bicycle", "Public Transport", "Vehicle", "Walking", "Other"),
                                         selected = c("Bicycle", "Public Transport", "Vehicle", "Walking", "Other")),
                      
                      selectInput("income_filter", "Select Income Group:", 
                                  choices = c("All", unique(na.omit(trips_hh$income_group)))),
                      
                      # Add year slider input with animation option
                      sliderInput("year_filter", "Filter by Year:", 
                                  min = min(as.numeric(substring(trips_hh$surveyperiod, 1, 4))), 
                                  max = max(as.numeric(substring(trips_hh$surveyperiod, 1, 4))), 
                                  value = c(min(as.numeric(substring(trips_hh$surveyperiod, 1, 4))),
                                            max(as.numeric(substring(trips_hh$surveyperiod, 1, 4)))),
                                  step = 1,  # Step by 1 year
                                  sep = "",  # No thousands separator
                                  animate = animationOptions(interval = 1000, loop = TRUE)), # Animation settings
                      wellPanel(
                        h4("Instruction"),
                        p("1. Select the travel modes you are interested in using the 'Groupings to plot' options."),
                        p("2. Choose a specific income group or view data for all income levels through 'Select Income Group'."),
                        p("3. Adjust the year range using the 'Filter by Year' slider to explore changes over time."),
                        p("4. Using the action button at the bottom of the slider to display an animation to see the dynamic changes."),
                        p("5. Hover to see details of each travel pattern."),
                        p("If you want to see the animation, please slide the year filter to '2012', and then click the button."),
                        p("Please note that the 'Count' has been transformed using logarithm.")
               ) # Using 'wellPanel' function to cover instructions in a box
             )
           ))
  ),
  # Create the "Household" tab
  tabPanel("Household",
           fluidPage(
             titlePanel("Household Dimension - Ridge or Sankey Plot"),
             p("Explore the distribution of household income, size, and vehicle ownership across different travel modes. You can switch between Ridge Plot and Sankey Plot, with each having different interactive ways."),
             
             sidebarLayout(
               sidebarPanel(
                 # Radio buttons for selecting the plot type
                 radioButtons("plot_type", "Select Plot Type:", 
                              choices = c("Ridge Plot" = "ridge", "Sankey Plot" = "sankey"),
                              selected = "ridge"),
                 
                 # Conditional UI to show controls based on the selected plot
                 uiOutput("plot_controls"),
                 width = 3
               ),
               
               mainPanel(
                 # Conditional panel for Ridge Plot
                 conditionalPanel(
                   condition = "input.plot_type == 'ridge'",
                   plotOutput("ridge_plot", height = "600px"),
                   wellPanel(
                     h4("Instruction"),
                     p("1. Select the mode of travel from the 'Select Mode' dropdown."),
                     p("2. Choose the household sizes you want to include using the checkboxes."),
                     p("3. Use the year slider to filter the data by year."),
                     p("4. The ridge plot will dynamically update to reflect your filter choices using the action button."),
                     p("If you want to see the animation, please slide the year filter to '2012', and then click the button.")
                   )
                 ),
                 
                 
                 # Conditional panel for Sankey Plot
                 conditionalPanel(
                   condition = "input.plot_type == 'sankey'",
                   plotlyOutput("sankey_plot", height = "600px"),
                   wellPanel(
                     h4("Instruction"),
                     p("1. Select the variables for the Sankey plot using the checkboxes."),
                     p("2. Choose combinations of household groups, vehicle groups, and income groups to visualize."),
                     p("3. The Sankey plot will show the flow between different categories based on your selections, hover to see the details.")
                   )
                 ),
                 width = 9
               )
             )
           )
  ),
  # Create a tab called "About" to see details about the app
tabPanel("About",
         fluidPage(
           titlePanel(HTML("<b>About This App</b>")), # Bold font
           
           p(HTML('This Shiny application provides an interactive way to analyze travel patterns in different time periods and 
                  the relationship between household characteristics and choices of travel modes using data from the <a href="https://discover.data.vic.gov.au/dataset/victorian-integrated-survey-of-travel-and-activity" target="_blank">Victorian Integrated Survey of Travel and Activity (VISTA) dataset</a>.')),
           
           h3(HTML("<b>Data Information</b>")),
           p("The dataset includes information on travel modes, household sizes, vehicle ownership, and household income of each family. 
             It was collected from households across Victoria from 2012 to 2020. I have already cleaned it in DEP Part, the final data used in this
             Data Visualization Project has 190,324 observations and 20 variables. Some variables, for example, income group, is mutated using K-means algorithm in data wrangling.
             If you want to see the original dataset, you can click the link above. This dataset is a tabular data, so it is not hard to download."),
           
           h3(HTML("<b>Questions to be solved</b>")),
           tags$ul(style = "font-size:18px;",
             tags$li("Do people choose different ways to travel depending on when they want to?"),
             tags$li("How do travel patterns vary in different household situations?
             (e.g. household size,household income,the number of vehicles they own)")),
           
           h3(HTML("<b>Features</b>")),
           tags$ul(style = "font-size:18px;",
             tags$li("Explore travel patterns over different time periods using the stacked bar chart in the 'Overview' tab."),
             tags$li("Analyze household dimensions using ridge plots or Sankey diagrams in the 'Household' tab."),
             tags$li("Apply various filters such as travel mode, household size, and income group to customize the visualizations."),
             tags$li("Click the action button at the right-bottom of the slider to display an animation.")
           ),
           
           h3(HTML("<b>Technologies</b>")),
           p("This app is built using the following technologies:"),
           tags$ul(style = "font-size:18px;",
             tags$li("Shiny for creating interactive web applications."),
             tags$li("ggplot2 for generating static plots."),
             tags$li("plotly for generating interactive charts and diagrams."),
             tags$li("ggridges for creating ridge plots.")
           )
         )
))



# Create a server
server <- function(input, output) {
  
  # Preprocess the surveyperiod column to extract the year (e.g., "2012-13" -> "2012")
  trips_hh$surveyperiod <- substring(trips_hh$surveyperiod, 1, 4)
  
  # Reactive data for bar chart filters
  filtered_data <- reactive({
    data <- trips_hh
    if (!is.null(input$mode_filter)) {
      data <- subset(data, linkmode %in% input$mode_filter)
    }
    if (input$income_filter != "All") {
      data <- subset(data, income_group == input$income_filter)
    }
    data <- subset(data, as.numeric(surveyperiod) >= input$year_filter[1] &
                     as.numeric(surveyperiod) <= input$year_filter[2])
    return(data)
  })
  
  # Render interactive stacked bar chart (Overview tab)
  output$interactive_bar_chart <- renderPlotly({
    summarized_data <- filtered_data() %>%
      group_by(daytype, time_period, linkmode) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(daytype, time_period) %>%
      mutate(proportion = count / sum(count)) %>%
      mutate(log_count = log10(ifelse(count == 0, 1, count)))  # Log-transform counts
    
    p <- ggplot(summarized_data, aes(x = time_period, y = log_count, fill = linkmode, text = paste0(
      "Travel Mode: ", linkmode, "<br>Count: ", log_count, "<br>Proportion: ", scales::percent(proportion)
    ))) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ daytype, scales = "free_y") +
      scale_fill_manual(values = c("Bicycle" = "#1B4F72", 
                                   "Other" = "#117A65", 
                                   "Public Transport" = "#9A7D0A", 
                                   "Vehicle" = "#B03A2E", 
                                   "Walking" = "#76448A")) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 16) +  # Increase the base font size
      labs(x = "Time Period", y = "Count", fill = "Travel Mode") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # UI controls depending on plot type
  output$plot_controls <- renderUI({
    if (input$plot_type == "ridge") {
      tagList(
        selectInput("ridge_mode_filter", "Select Mode:", choices = unique(trips_hh$linkmode)),
        checkboxGroupInput("ridge_hhsize_filter", "Select Household Size:",
                           choices = sort(unique(trips_hh$hhsize)), 
                           selected = sort(unique(trips_hh$hhsize))),
        
        # Add year slider input with animation option for Ridge Plot
        sliderInput("ridge_year_filter", "Filter by Year:", 
                    min = min(as.numeric(substring(trips_hh$surveyperiod, 1, 4))), 
                    max = max(as.numeric(substring(trips_hh$surveyperiod, 1, 4))), 
                    value = c(min(as.numeric(substring(trips_hh$surveyperiod, 1, 4))),
                              max(as.numeric(substring(trips_hh$surveyperiod, 1, 4)))),
                    step = 1,  # Step by 1 year
                    sep = "",  # No thousands separator
                    animate = animationOptions(interval = 1000, loop = TRUE))  # Enable animation
      )
    } else if (input$plot_type == "sankey") {
      tagList(
        checkboxGroupInput("sankey_vars", "Select Variables for Sankey Plot:", 
                           choices = c("Income Group" = "income_group", "Household Group" = "size", 
                                       "Vehicle Group" = "vehicle_no", "Travel Mode" = "linkmode"),
                           selected = c("income_group", "size", "vehicle_no", "linkmode"))
      )
    }
  })
  
  # Reactive data for Ridge Plot
  filtered_ridge_data <- reactive({
    data <- trips_hh
    if (input$plot_type == "ridge") {
      if (input$ridge_mode_filter != "All") {
        data <- subset(data, linkmode == input$ridge_mode_filter)
      }
      data <- subset(data, hhsize %in% input$ridge_hhsize_filter)
      # Filter by year based on ridge_year_filter
      data <- subset(data, as.numeric(surveyperiod) >= input$ridge_year_filter[1] &
                       as.numeric(surveyperiod) <= input$ridge_year_filter[2])
    }
    return(data)
  })
  
  # Reactive data for Sankey Plot
  filtered_sankey_data <- reactive({
    data <- trips_hh
    if (input$plot_type == "sankey") {
      selected_vars <- input$sankey_vars
      data <- data %>%
        group_by(across(all_of(selected_vars))) %>%
        summarise(count = n()) %>%
        ungroup()
    }
    return(data)
  })
  
  # Render Ridge Plot (ggplot2)
  output$ridge_plot <- renderPlot({
    if (input$plot_type == "ridge") {
      ridge_data <- filtered_ridge_data()
      ggplot(ridge_data, aes(x = hhinc, y = factor(hhsize), fill = factor(totalvehs))) +
        geom_density_ridges(alpha = 0.5) +
        labs(title = "Ridge Plot of Travel Mode by Household Size, Number of Vehicles, and Income",
             x = "Household Income", 
             y = "Household Size",
             fill = "Number of Vehicles") +
        theme_minimal(base_size = 16) +  # Increased font size here
        scale_fill_viridis_d()
    }
  })
  
  # Render Sankey Plot (plotly)
  output$sankey_plot <- renderPlotly({
    if (input$plot_type == "sankey") {
      sankey_data <- filtered_sankey_data()
      
      # Create nodes and mapping indices for Sankey
      all_vars <- unique(unlist(sankey_data))
      node_labels <- as.character(all_vars)
      
      # Create the source and target mappings based on the selected variables
      source_target_pairs <- lapply(1:(length(input$sankey_vars) - 1), function(i) {
        source <- match(sankey_data[[input$sankey_vars[i]]], node_labels) - 1
        target <- match(sankey_data[[input$sankey_vars[i + 1]]], node_labels) - 1
        list(source = source, target = target)
      })
      
      # Flatten source-target lists
      sources <- unlist(lapply(source_target_pairs, `[[`, "source"))
      targets <- unlist(lapply(source_target_pairs, `[[`, "target"))
      
      # Create the Sankey plot
      plot_ly(
        type = "sankey",
        node = list(
          label = node_labels,
          pad = 15,
          thickness = 20,
          line = list(color = "black", width = 0.5),
          font = list(size = 16)  
        ),
        link = list(
          source = sources,
          target = targets,
          value = rep(sankey_data$count, length(input$sankey_vars) - 1)
        )
      ) %>%
        layout(
          title = "Sankey Diagram of Household Groups, Vehicle Groups, and Income Groups",
          font = list(size = 16),
          margin = list(t = 100) # Move the title a little bit down 
        )
    }
  })
}

# Run the application
shinyApp(ui, server)
