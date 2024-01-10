# Install required packages if not already installed
# install.packages(c("shiny", "ggplot2", "dplyr", "tidyr"))

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Example dataset based on provided CTR data
example_data <- data.frame(
  Ad_Placement = paste0("Day ", 1:10),
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Create the UI
ui <- fluidPage(
  titlePanel("Ad Placement CTR Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a CSV file", accept = ".csv"),
      actionButton("analyze_btn", "Analyze")
    ),
    
    mainPanel(
      plotOutput("ctr_plot"),
      verbatimTextOutput("result_output")
    )
  )
)

# Create the server
server <- function(input, output) {
  data <- reactive({
    # Check if a file is uploaded
    if (!is.null(input$file)) {
      # Read the uploaded CSV file with the correct separator
      df <- read.csv(input$file$datapath, sep = ";", header = TRUE)
      # Reshape the data to long format
      df_long <- tidyr::gather(df, key = "Ad_Location", value = "CTR", -Ad_Placement)
      return(df_long)
    } else {
      # Use the example dataset if no file is uploaded
      return(tidyr::gather(example_data, key = "Ad_Location", value = "CTR", -Ad_Placement))
    }
  })
  
  output$ctr_plot <- renderPlot({
    ggplot(data(), aes(x = Ad_Placement, y = CTR, fill = Ad_Location)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      labs(title = "CTR Performance by Ad Placement Location",
           x = "Ad Placement",
           y = "Click-Through Rate (%)",
           fill = "Ad Placement Location") +
      theme_minimal()
  })
  
  output$result_output <- renderPrint({
    # Perform statistical analysis if needed
    result_left_center <- t.test(data()$CTR[data()$Ad_Location == "Left_Sidebar"],
                                 data()$CTR[data()$Ad_Location == "Center_Page"])
    result_center_right <- t.test(data()$CTR[data()$Ad_Location == "Center_Page"],
                                  data()$CTR[data()$Ad_Location == "Right_Sidebar"])
    result_left_right <- t.test(data()$CTR[data()$Ad_Location == "Left_Sidebar"],
                                data()$CTR[data()$Ad_Location == "Right_Sidebar"])
    
    # Display results
    cat("Left Sidebar vs. Center Page:\n",
        "P-value:", signif(result_left_center$p.value, digits = 4),
        "\nStatistically Significant:", ifelse(result_left_center$p.value < 0.05, "Yes", "No"),
        "\n\nCenter Page vs. Right Sidebar:\n",
        "P-value:", signif(result_center_right$p.value, digits = 4),
        "\nStatistically Significant:", ifelse(result_center_right$p.value < 0.05, "Yes", "No"),
        "\n\nLeft Sidebar vs. Right Sidebar:\n",
        "P-value:", signif(result_left_right$p.value, digits = 4),
        "\nStatistically Significant:", ifelse(result_left_right$p.value < 0.05, "Yes", "No"))
  })
}

# Run the application
shinyApp(ui, server)
