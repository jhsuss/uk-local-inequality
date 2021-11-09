library(shiny)
library(sf)
library(tidyverse)
library(DT)
library(plotly)
library(waiter)
library(shinyjs)
library(htmlwidgets)
library(mapdeck)


ui <- navbarPage(
    
    # title of app 
    title = "Local inequality in the UK", 
    id = "home",
    
    
    tabPanel("Map", 
             value = "map",
             div(class = "outer",
                 
                 tags$head(
                     # Include custom CSS
                     includeCSS("styles.css"),
                     #includeScript("gomap.js")
                     useShinyjs(),
                     useWaiter(),
                     waiterPreloader()
                     
                     #waiterOnBusy(),
                     
                 ),
                 
                 # leafletOutput(
                 #     "map" 
                 #     ,width = "100%" 
                 #     ,height = "100%"
                 # ),
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
                     
                     # options: income / wealth / housing
                     # options: 
                     fluidRow(
                         column(6, actionButton("options", "More options")),
                         column(6, downloadButton("download", "Download data"))
                         ),
                     
                     # TODO draw your own area? cf. nebular.gl (R port?)
                     
                     # TODO
                     # distribution of house values in area
                     #withSpinnter()plotlyOutput("house_values", height = 200),
                     # plotOutput("scatterCollegeIncome", height = 250)
                 ),
                 
                 tags$div(id = "cite",
                          'Data compiled for ', tags$em('Measuring local, salient inequality in the UK'), ' by Joel H Suss.'
                 ),
                 
             )
    ), # end Map tab
    
    tabPanel("Data", value = "data",
             fluidRow(
                 # column(3,
                 #        selectizeInput(
                 #            "lad",
                 #            "Local Authority",
                 #            # c("All states"="",
                 #            #   structure(state.abb, names=state.name),
                 #            #   "Washington, DC"="DC"
                 #            #   ),
                 #            multiple = TRUE)
                 # ),
                 # column(3,
                 #        conditionalPanel(
                 #            "input.states",
                 #            selectInput(
                 #                "cities", "Cities",
                 #                c("All cities"=""),
                 #                multiple=TRUE)
                 #        )
                 #),
             hr(),
             dataTableOutput("ineq_table")
             #)
             )
    ), # end Data panel

    tabPanel(
        "About", value = "about",
        #useMarkdown()
        
        ) # end About panel
    
    )

# Define server logic required to map inequality and allow for data download
server <- function(input, output, session) {
    
    # introduce the waiter
    w <- Waiter$new()
    
    # return lon lat, bounding box and zoom of current view
    observeEvent({input$map_view_change},{
        print(input$map_view_change)
        
    })
    
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    # area_bounds <- reactive({
    #     
    #     # if (is.null(input$map_bounds))
    #     #     return(df()[FALSE,])
    #     
    #     req(!is.null(input$map_bounds))
    # 
    # 
    #     bounds <- input$map_bounds
    #     # rename to match st_bbox requirements
    #     names(bounds) <- c(
    #         "ymax","xmin","ymin","xmax"
    #     )
    #     
    # })

    # sf data frame of inequality at chosen spatial level
    df <- reactive({
        
        req(!is.null(input$level))
        
        map_data[[input$level]] %>%
            select(all_of(input$measure), all_of(vars_to_display)) %>% 
            # add popup information 
            mutate(
                info = paste0(
                    "<span><b>",name,"</b></span>"
                    )
            )
            # TODO reduce size based on zoom?
            #st_crop(y = area_bounds())
        
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
                # , legend_options = list(
                #     list( title = input$measure %>% str_to_title)
                #     #, css = "max-height: 100px;" ## css applied 
                # )
                , digits = 3
                , update_view = FALSE
            ) 
        
        w$hide()
    })
    
    
    
    
    # OPTIONS dialog
    
    # observer to display options
    observeEvent(input$options,{
        showModal(options_modal())
    })
    
    options_modal <- function() {
        
        tagList(
            modalDialog(
                title = h3("Select options"),
                size = "m",
                easyclose = T,
                
                fluidPage(),
                
                footer = tagList(
                    modalButton("Done")
                )
            )
        )
        
    }
    
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
                
                fluidPage(),
                
                footer = tagList(
                    modalButton("Cancel"),
                    downloadButton(outputId = "data_download",
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
            
            df <- map_data[[input$level]] %>% 
                st_drop_geometry %>% 
                as.data.frame()
            
            # save on local machine
            write_csv(df, file)
            
            # shiny alert popup to indicate success
            #shinyalert("Saved!", "Data downloaded successfully", type = "success")
            
            # remove download modalDialog
            removeModal()
            
        }
        
        
    )
    
    
    # popups with info on area
    # Show a popup at the given location
    show_popup <- function(id, lat, lng) {
        
        # selected_area <- df()[df()$id == id,] %>% 
        #     st_drop_geometry()
        
        selected_area <- df()[id,] %>% 
            st_drop_geometry()
        
        print(selected_area)
        
        content <- as.character(
            tagList(
                tags$h4(
                    # display inequality value
                    input$measure,
                    sprintf(": %s%%",selected_area[[input$measure]])
                    ),
                tags$strong(
                    HTML(
                        sprintf(
                        "%s, %s %s",
                        selected_area$id, 
                        selected_area$id, 
                        selected_area$id
                        ))), 
                tags$br(),
                sprintf("Median household value: %s", paste0(enc2utf8("\u00A3"),format(selected_area$med, big.mark = ","))), 
                tags$br(),
                sprintf("Percent of adults with degree: %s%%", as.integer(selected_area$sd)), 
                tags$br(),
                sprintf("Adult population: %s", selected_area$med)
        ))
        print(content)
        leafletProxy("map") %>% addPopups(lng, lat, content#, layerId = ~ id
                                          )
    }

    # When map is clicked, show a popup with area info
    # observe({
    #     #leafletProxy("map") %>% clearPopups()
    #     
    #     #event <- input$map_shape_click
    #     #event <- input$map_glify_click
    #     event <- input$map_polygon_click
    #     
    #     event <- jsonify::from_json(as.character(event))
    #     #event <- event[c("index","lon","lat")]
    #     #print(event)
    #     
    #     if (is.null(event))
    #         return()
    # 
    #     isolate({
    #         #show_popup(event$id, event$lat, event$lng)
    #         #show_popup(event$data$id, event$lat, event$lng)
    #         show_popup(event$index, event$lat, event$lon)
    #     })
    # })
    
    
    ## Data Explorer ###########################################
    
    # observe({
    #     cities <- if (is.null(input$states)) character(0) else {
    #         filter(cleantable, State %in% input$states) %>%
    #             `$`('City') %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$cities[input$cities %in% cities])
    #     updateSelectizeInput(session, "cities", choices = cities,
    #                          selected = stillSelected, server = TRUE)
    # })
    # 
    # observe({
    #     zipcodes <- if (is.null(input$states)) character(0) else {
    #         cleantable %>%
    #             filter(State %in% input$states,
    #                    is.null(input$cities) | City %in% input$cities) %>%
    #             `$`('Zipcode') %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    #     updateSelectizeInput(session, "zipcodes", choices = zipcodes,
    #                          selected = stillSelected, server = TRUE)
    # })
    # 
    # observe({
    #     if (is.null(input$goto))
    #         return()
    #     isolate({
    #         map <- leafletProxy("map")
    #         map %>% clearPopups()
    #         dist <- 0.5
    #         zip <- input$goto$zip
    #         lat <- input$goto$lat
    #         lng <- input$goto$lng
    #         showZipcodePopup(zip, lat, lng)
    #         map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    #     })
    # })
    # 
    # output$ineq_table <- renderDataTable({
    #     # df <- cleantable %>%
    #     #     filter(
    #     #         Score >= input$minScore,
    #     #         Score <= input$maxScore,
    #     #         is.null(input$states) | State %in% input$states,
    #     #         is.null(input$cities) | City %in% input$cities,
    #     #         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
    #     #     ) %>%
    #     #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #     # action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    # 
    #     datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    # })
    
    # TODO load raw house values from data using filter
    # h/t https://stackoverflow.com/questions/15967226/skip-some-rows-in-read-csv-in-r/15967406#15967406
    
    # output$house_values <- renderPlotly({
    #     # TODO this needs to update upon selection of LSOA
    #     # TODO add label for total N
    #     # TODO add line of summary stats (e.g. mean, med, s.d., skew)
    #     print(bbox())
    #     g <- ggplot(lsoa) + 
    #         geom_histogram(mapping = aes_string(x = input$meaure)) 
    #     
    #     g
    #     #ggplotly(g)
    #     
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
