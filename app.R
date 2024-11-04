## LOAD PACKAGES
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(rsconnect)
theme_set(theme_bw())


## DATA PREPROCESSING
companies=read.csv("https://raw.githubusercontent.com/jasonjclark/stat436deadline2/refs/heads/main/publiccompanies.csv")%>%
  # Use of "&" as well as "and" created duplicate industries, prefer usage of "and"
  mutate(industry = case_when(
    industry == "Aerospace & Defense" ~ "Aerospace and Defense",
    industry == "Metals & Mining" ~ "Metals and Mining",
    TRUE ~ industry
  ))



## PLOTTING FUNCTION
boxplot=function(data, esg_col, esg_label){
  
  # Generate static plot
  p=ggplot(data, aes(x = industry, y = .data[[esg_col]], fill = industry)) + # Industry on X-axis, selected ESG score on y
    
    # Box plot element, 1 per industry
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Remove outliers
    
    # Jitter element, 1 per company
    geom_jitter(aes(text = paste("Company:", name, "<br>", esg_label, ":", .data[[esg_col]])), # For displaying when hovering
                width = 0.2,alpha = 0.5, size = 1) +  # Style adjustments
    labs(
      title = paste(esg_label, "Distribution by Industry"),
      x = "Industry",
      y = esg_label,
      fill = "Industry"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # adjust to fit all labels
      legend.position = "none"
    )
  
  # Convert to plotly for interactivity
  p_plotly=ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      xaxis = list(title = "Industry"),
      yaxis = list(title = esg_label)
    )
  
  return(p_plotly)
}



## UI
ui=fluidPage(
  # Title
  titlePanel("ESG Score Boxplots by Industry"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      # ESG Category selection
      selectInput(
        inputId = "esg_category",
        label = "Select ESG Category:",
        choices = c("Environmental" = "environment_score",
                    "Social" = "social_score",
                    "Governance" = "governance_score",
                    "Total ESG" = "total_score"),
        selected = "total_score"
      ),
      
      # Industry selection
      selectizeInput(
        inputId = "industries",
        label = "Select Industries:",
        choices = sort(unique(companies$industry)),
        selected = sort(unique(companies$industry))[1:5], #Set first 5 as default
        multiple = TRUE, # Allow multi-select
        options = list(placeholder = 'Select one or more industries')
      ),
      
      helpText("When examining ESG scores for sustainable investing, it's important to know the context of a score comparatively to similar companies. This tool illustrates the distribution of ESG scores within distinct industries, allowing users to examine how companies are performing relative to their peers. Use the above selectors to modify both which score category you are viewing, as well as which companies you wish to be displayed. Hover over points to see which company is being displayed, as well as that company's score.")
    ),
    
    # Main panel to display plot
    mainPanel(
      plotlyOutput(outputId = "boxplot", height="700px")
    )
  )
)

## SERVER
server=function(input, output) {
  
  # Reactive expression to filter data based on selected industries
  filtered_data=reactive({
    companies %>%
      filter(industry %in% input$industries) %>%
      select(industry, environment_score, social_score, governance_score, total_score, name)
  })
  
  # Render boxplot
  output$boxplot=renderPlotly({
    data=filtered_data()
    
    # Handling cases with no data
    validate(
      need(nrow(data) > 0, "No data available for the selected industries.")
    )
    
    # Get selected ESG category
    esg_col=input$esg_category
    
    #Update labels to fit selected ESG category
    esg_label=switch(esg_col,
                     "environment_score" = "Environmental Score",
                     "social_score" = "Social Score",
                     "governance_score" = "Governance Score",
                     "total_score" = "Total ESG Score")
    
    # Plot
    boxplot(data, esg_col, esg_label)
  })
}

#Run
shinyApp(ui, server)