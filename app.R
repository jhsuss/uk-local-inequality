#library(shiny)
library(sf)
library(tidyverse)
library(DT)
library(plotly)
library(waiter)
library(shinyjs)
library(htmlwidgets)
library(mapdeck)
library(scales)

ui <- navbarPage(
  
  # title of app 
  title = "Local economic inequality in the UK", 
  id = "home",
  
  tabPanel("Map", 
           value = "map",
           div(class = "outer",
               
               tags$head(
                 # add google analytics
                 includeHTML("google-analytics.html"),
                 # Include custom CSS
                 includeCSS("styles.css"),
                 useShinyjs(),
                 useWaiter(),
                 waiterPreloader()
               ),
               
               mapdeckOutput(
                 outputId = 'map',
                 width = "100%",
                 height = "100%"
               ),
               
               # draggable panel to select inequality measure and level
               absolutePanel(
                 id = "controls", 
                 class = "panel panel-default", 
                 fixed = TRUE,
                 draggable = TRUE, 
                 top = 60, 
                 left = "auto", 
                 right = 20, 
                 bottom = "auto",
                 width = 330, 
                 height = "auto",
                 
                 h2("Inequality explorer"),
                 
                 selectizeInput("level", "Level", levs, selected = "bua"),
                 
                 selectizeInput(
                   "measure", 
                   "Measure", 
                   ineq_measures
                 ),
                 
                 # options: 
                 fluidRow(
                   column(6, actionButton("download", "Download data", class = "download"))
                 ),
               ),
               
               tags$div(id = "cite",
                        'Data compiled for ', tags$em('Measuring local, salient inequality in the UK'), ' by Joel H Suss.'
               ),
               
           )
  )
  
)

# Define server logic required to map inequality and allow for data download
server <- function(input, output, session) {
  
  # introduce the waiter
  w <- Waiter$new()
  
  # return lon lat, bounding box and zoom of current view
  observeEvent({input$map_view_change},{
    print(input$map_view_change)
    
  })
  
  # sf data frame of inequality at chosen spatial level
  df <- reactive({
    
    req(!is.null(input$level))
    
    map_data[[input$level]] %>%
      select(all_of(input$measure), all_of(vars_to_display)) %>% 
      # add popup information 
      mutate(
        info = paste0(
          "<span><b>",name,"</b></span>",
          "<br> N = ",houses %>% comma(),
          "<br>", input$measure, " ", .data[[input$measure]],
          "<br> Median house price = ", med %>% comma()
        )
      )
    
  })
  
  ## Interactive Map ###########################################
  
  # Create the static map (mapdeck version)
  output$map <- renderMapdeck({
    mapdeck(
      style = mapdeck_style('dark'),
      location = c(-2.93, 54.3), 
      zoom = 5 
    )
  })
  
  observe({
    
    # show waiter
    w$show()
    
    mapdeck_update(map_id = 'map') %>%
      add_polygon(
        data = df()
        , fill_colour = input$measure 
        , layer_id = "inequality"
        , fill_opacity = 0.7
        #, colour_range = colourvalues::colour_values(1:6, palette = "plasma")
        , palette = "plasma"
        , auto_highlight = T
        , highlight_colour = "#FFFFFFFF"
        , tooltip = "info"
        , id = "id"
        , legend = T
        , digits = 3
        , update_view = FALSE
      ) 
    
    w$hide()
  })

  # DATA DOWNLOAD dialog
  
  # observer 
  observeEvent(input$download,{
    showModal(data_modal())
  })
  
  data_modal <- function() {
    
    tagList(
      modalDialog(
        title = h3("Download inequality data"),
        size = "m",
        easyclose = T,
        
        fluidPage(
          renderText({
            paste0(
              "Click to download a .csv file for all inequality measures at the chosen spatial level." 
              #,levs %>% filter(input$level),
              #" level"
            )
          })
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          downloadButton(
            outputId = "data_download",
            label = "Download"
          )
        )
      )
    )
    
  }
  
  output$data_download <- downloadHandler(
    filename = paste0("data.csv"),
    content = function(file) {
      
      req(!is.null(input$level))
      
      # tidy data for download
      df <- map_data[[input$level]] %>% 
        st_drop_geometry %>%
        as.data.frame()
      
      df <- df[!is.na(df[[input$measure]]),]
      
      # save on local machine
      write_csv(df, file = file)
      
      # remove download modalDialog
      removeModal()
      
    }
    
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
