# import packages
library('shiny')
library('ggplot2')
library('readr')
library('clock')
library('plotly')
library('tidyverse')
library('rgdal')
library('raster')
library('sf')
library('tmap')
library('mapview')

# read data
cc_data <- read_csv("cc_data.csv")
loyalty_data <- read_csv("loyalty_data.csv")
total_match <- read_csv("total_match.csv")
gps_sf <- readRDS("gps_sf.rds")

# data processing
transaction_data = merge(cc_data, loyalty_data, all=TRUE)

ui <- navbarPage("VAST Chanllenge 2021 MC2 Menu",
    navbarMenu("Location",
               tabPanel("Popuplar Locations Barchart", 
                        sidebarLayout(
                            sidebarPanel(width = 2, 
                                         fluid = TRUE,
                                selectInput(inputId = "date_barchart",
                                            label = "Date",
                                            choices = unique(substr(transaction_data$timestamp, 1, 10)))
                            ),
                            
                            mainPanel(width = 9, 
                                      fluid = TRUE,
                                plotOutput("location_barchart",
                                           width = "600px",
                                           height = "600px")
                            )
                        )
               ),
               
               tabPanel("Popular Locations Heatmap", 
                        sidebarLayout(
                            sidebarPanel(width = 2, 
                                         fluid = TRUE,
                                selectInput(inputId = "date_heatmap",
                                            label = "Date",
                                            choices = unique(substr(cc_data$timestamp, 1, 10)))
                            ),
                            
                            mainPanel(width = 9, 
                                      fixed = TRUE,
                                textOutput("heatmap_text"),
                                plotlyOutput("location_heatmap",
                                             width = "600px",
                                             height = "600px"),
                                textOutput("popular_text"),
                                plotOutput("popular",
                                           width = "600px",
                                           height = "600px")
                            )
                        ) 
               ),
               
               tabPanel("Location Sales Boxplot", 
                        sidebarLayout(
                            sidebarPanel(width = 2, 
                                         fluid = TRUE,
                                selectInput(inputId = "location",
                                            label = "Location",
                                            choices = unique(transaction_data$location))
                            ),
                            
                            mainPanel(width = 9, 
                                      fluid = TRUE,
                                plotlyOutput("location_boxplot",
                                             width = "600px",
                                             height = "600px")
                            )
                        )
               )
    ),
    
    navbarMenu("GPS Record",
               tabPanel("GPS Record Timeline",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         fluid = TRUE,
                                selectInput(inputId = "day_timeline",
                                            label = "Day",
                                            choices = unique(gps_sf$day))
                            ),

                            mainPanel(width = 9,
                                      fluid = TRUE,
                                plotlyOutput("gps_timeline",
                                             width = "600px",
                                             height = "600px")
                            )
                        )
               ),

               tabPanel("GPS Record Path(Time Period)",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         fluid = TRUE,
                                selectInput(inputId = "carid",
                                            label = "Car ID",
                                            choices = sort(unique(gps_sf$id))),
                                selectInput(inputId = "day_path",
                                            label = "Day",
                                            choices = unique(gps_sf$day)),
                                selectInput(inputId = "hour_start",
                                            label = "Start Hour",
                                            choices = c(0:24),
                                            selected = 0),
                                selectInput(inputId = "hour_end",
                                            label = "End Hour",
                                            choices = c(0:24),
                                            selected = 24)
                            ),

                            mainPanel(width = 9,
                                      fluid = TRUE,
                                textOutput("error"),
                                plotOutput("path",
                                           width = "700px",
                                           height = "550px")
                            )
                        )
               ),

               tabPanel("GPS Record Path(Timestamp)",
                        sidebarLayout(
                            sidebarPanel(width = 2,
                                         fluid = TRUE,
                                         selectInput(inputId = "day_timestamp",
                                                     label = "Day",
                                                     choices = unique(gps_sf$day)),
                                         selectInput(inputId = "hour",
                                                     label = "Hour",
                                                     choices = c(0:24),
                                                     selected = 12),
                                         selectInput(inputId = "minute",
                                                     label = "Minute",
                                                     choices = c(0:59),
                                                     selected = 0)
                            ),

                            mainPanel(width = 9,
                                      fluid = TRUE,
                                      textOutput("error_timestamp"),
                                      plotOutput("path_timestamp",
                                                 width = "700px",
                                                 height = "550px"),
                                      DT::dataTableOutput(outputId = "cctable")
                            )
                        )
               )
    ),

    tabPanel("Total Data Match",
               mainPanel(width = 9,
                         fluid = TRUE,
                         DT::dataTableOutput(outputId = "match")
               )
    )
)

server <- function(input, output) {
    reorder_size <- function(x) {
        factor(x, levels = names(sort(table(x))))
    }   
    
    # bar chart for number of transactions in different locations
    output$location_barchart <- renderPlot({
        transaction_data <- transaction_data %>%
            filter(substr(timestamp, 1, 10) == input$date_barchart) 
        
        ggplot(data = transaction_data, 
               aes(y = reorder_size(location))) + 
            geom_bar(fill = "steelblue4") + 
            xlab("Number of Transactions") + 
            ylab("Location") 
    })
    
    # bar chart for number of transactions in different locations (All Dates)
    output$popular <- renderPlot({
        ggplot(data = transaction_data, 
               aes(y = reorder_size(location))) + 
            geom_bar(fill = "steelblue4") + 
            xlab("Number of Transactions") + 
            ylab("Location") 
    })
    
    output$heatmap_text <- renderText({"Heatmap for different locations in different dates"})
    output$popular_text <- renderText({"Barchart for most popolar locations in 14 days"})
    
    # heat map for credit card transactions in different locations
    output$location_heatmap <- renderPlotly({
        cc_data <- cc_data %>%
            filter(substr(timestamp, 1, 10) == input$date_heatmap)
        cc_data$timestamp <- date_time_parse(cc_data$timestamp,
                                             zone = "",
                                             format = "%m/%d/%Y %H:%M")
        cc_data$hour <- get_hour(cc_data$timestamp)
        cc_data$day <- get_day(cc_data$timestamp)
        df_hmap <- data.frame(cc_data$hour, cc_data$location)
        colnames(df_hmap) <- c("Hour", "Location")
        
        p <- ggplot(df_hmap, 
                    aes(x = Hour, 
                        y = Location)) + 
            geom_bin2d() + 
            scale_fill_gradient(low = "steelblue1", 
                                high = "steelblue4") + 
            scale_x_continuous("Hour", 
                               labels = as.character(cc_data$hour), 
                               breaks = cc_data$hour) +
            xlab("Hour") + 
            ylab("Location") 
        ggplotly(p)
    })
    
    # box plot for transaction price distribution in different locations
    output$location_boxplot <- renderPlotly({
        transaction_data$timestamp <- strptime(transaction_data$timestamp, 
                                               format = '%m/%d/%Y')
        transaction_data <- transaction_data %>%
            mutate(transaction_data, 
                   day = strftime(transaction_data$timestamp, 
                                  format = '%m/%d'))
        transaction_data <- transaction_data %>%
            filter(location == input$location) 
        
        p <- ggplot(data = transaction_data, 
               aes(x = day,
                   y = price,
                   text = paste("last4ccnum:", last4ccnum,
                                "<br>loyaltynum:", loyaltynum))) + 
            geom_boxplot(outlier.shape = NA) + 
            geom_point(position = "jitter", 
                       size = 0.4) + 
            xlab("Day") + 
            ylab("Price") 
        ggplotly(p)
    })
    
    # gps record timeline for different cars
    output$gps_timeline <- renderPlotly({
        gps_sf <- gps_sf %>%
            filter(day == input$day_timeline)
        
        p <- ggplot(gps_sf,
               aes(x = Timestamp,
                   y = factor(id),
                   text = paste("car id:", as.character(id),
                                "<br>timestamp:", Timestamp))) +
            geom_point(size = 2,
                       color = "skyblue4") +
            ylab("Car id")
        ggplotly(p, tooltip = "text")
    })
    
    # path plot for gps record for different cars in different time period
    output$path <- renderPlot({
        gps_sf <- gps_sf %>%
            filter(id == input$carid,
                   day == input$day_path,
                   as.integer(hour) >= as.integer(input$hour_start),
                   as.integer(hour) <= as.integer(input$hour_end))
        if(length(gps_sf$id) == 0) {
            output$error <- renderText({"No related record found in this time period!"})
        }
        else {
            gps_sf <- gps_sf %>%
                group_by(id, day) %>%
                summarize(m = mean(Timestamp),
                          do_union=FALSE) %>%
                st_cast("LINESTRING")
            
            output$error <- renderText({""})
            bgmap <- raster("MC2-tourist.tif")
            tm_shape(bgmap) +
                tm_rgb(bgmap, r = 1, g = 2, b = 3,
                       alpha = NA,
                       saturation = 1,
                       interpolate = TRUE,
                       max.value = 255) +
                tm_shape(gps_sf) +
                tm_lines("skyblue4")
        }
    })
    
    # path plot for gps record of all cars in a specific timestamp
    output$path_timestamp <- renderPlot({
        gps_sf <- gps_sf %>%
            filter(day == input$day_timestamp,
                   hour == input$hour,
                   as.integer(minute) <= min(59, as.integer(input$minute) + 3),
                   as.integer(minute) >= max(0, as.integer(input$minute) - 3))
        gps_sf$id <- factor(gps_sf$id)
        
        if(length(gps_sf$id) == 0) {
            output$error_timestamp <- renderText({"No related record found in this time period!"})
        }
        else {
            output$error_timestamp <- renderText({""})
            bgmap <- raster("MC2-tourist.tif")
            tm_shape(bgmap) +
                tm_rgb(bgmap, r = 1, g = 2, b = 3,
                       alpha = NA,
                       saturation = 1,
                       interpolate = TRUE,
                       max.value = 255) +
                tm_shape(gps_sf) +
                tm_dots(col = "id",
                        size = 0.6)
        }
    })
    
    # credit card data table
    output$cctable <- DT::renderDataTable({
        DT::datatable(data = cc_data,
                      options= list(pageLength = 10),
                      rownames = FALSE)
    })

    # total card data and owner match data table
    output$match <- DT::renderDataTable({
        DT::datatable(data = total_match,
                      options= list(pageLength = 10),
                      rownames = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)