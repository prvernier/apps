library(sf)
library(DT)
library(amt)
#library(bslib)
library(leaflet)
library(shinyjs)
#library(markdown)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)

options(shiny.maxRequestSize=100*1024^2) 
options(DT.options = list(scrollX = TRUE))

col_yrs6 <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')


ui = dashboardPage(skin="blue",
  
  dashboardHeader(title="BEACONs Movement Explorer", titleWidth = 387),
  
  dashboardSidebar(
    sidebarMenu(id="tabs",
      menuItem("Welcome", tabName="home", icon=icon("th")),
      menuItem("Select study area", tabName="select", icon=icon("arrow-pointer")),
      radioButtons("selectInput", "Select source dataset:", choices = list("Use demo dataset" = "usedemo", "Upload your own data" = "usedata"), 
        selected = character(0), inline = FALSE),
      conditionalPanel(
        condition="input.selectInput=='usedata'",
        fileInput("csv1", "Movement data (csv):", accept=".csv"),
      ),
      actionButton("getButton", "Load data"),
      hr(),
      sliderInput("daterange", "Select year(s):", min=2020, max=2025, value=c(2020,2025), sep="")
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
    tabItems(
      tabItem(tabName="home",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Overview", includeMarkdown("docs/overview.md")),
            tabPanel("User guide", includeMarkdown("docs/user_guide.md")),
            tabPanel("Data requirements", includeMarkdown("docs/datasets.md"))
          )
        )
      ),
      tabItem(tabName="select",
        fluidRow(
          tabBox(id = "one", width="12",
            tabPanel("Mapview", leafletOutput("map1", height=800) |> withSpinner()),
            tabPanel("Movement data", DTOutput("gps_data")),
            tabPanel("Sampling duration", plotOutput("duration")),
            tabPanel("Sampling rates", DTOutput("sampling_rates")),
          )
        )
      )     
    )
  )
)

server = function(input, output, session) {

  # Read gps movement data
  gps_csv <- eventReactive(list(input$selectInput,input$csv1), {
    req(input$selectInput)  # Ensure `selectInput` is not NULL
    if (input$selectInput == "usedemo") {
      readr::read_csv('www/demo_gps.csv') |>
          mutate(year=year(time), yday=yday(time))
    } else if (input$selectInput == "usedata") {
      req(input$csv1)
      readr::read_csv(input$csv1$datapath) |>
        mutate(year=year(time), yday=yday(time))
    }
  })

  studyarea <- reactive({
    trk <- gps_csv() |>
      make_track(.x=long, .y=lat, crs = 4326) #|>
      #transform_coords(3578)
    aoi <- hr_kde(trk, levels=0.95) |> hr_isopleths() #|>
      #st_transform(4326)
  })

  # Update choices for caribou individuals input based on input movement data
  observeEvent(c(input$selectInput, input$csv1), {
    x <- gps_csv()
    ids <- as.character(sort(unique(x$id)))
    updateSelectInput(session, "caribou", choices=ids, selected=ids[1])
    updateSliderInput(session, "daterange", min=min(x$year), max=max(x$year), value=c(min(x$year),max(x$year)))
  })

  # Create tracks using amt package
  trk_all <- eventReactive(input$getButton, {
    day1 <- yday(as.Date("Jan-01", "%b-%d"))
    x <- gps_csv() |>
      make_track(.x=long, .y=lat, .t=time, id = id, long=long, lat=lat, elev=elev, crs = 4326) #|>
      #transform_coords(crs_to = 3578)
    x |> mutate(sl_ = step_lengths(x), 
      speed = speed(x),
      yday = yday(t_),
      year = year(t_)) |>
      mutate(yday = ifelse(yday>=day1 & yday<=366, yday-day1+1, 366-day1+1+yday), 
        nsd = nsd(x))
  })
  
  # Output 'GPS data' to table
  output$gps_data <- renderDT({
    req(input$getButton)
    datatable(gps_csv())
  })

  # Output 'Sampling duration' to plot
  output$duration <- renderPlot({
   x <- gps_csv() |>
      mutate(id = as.factor(id), year = year(time))
    ggplot(data=x, aes(x=time, y=id)) +
      geom_path(size=1) +
      xlab('Time') + ylab('Collar ID') +
      theme(legend.position = 'none') +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 13))
  }, height=600)

  # Output 'Sampling rates' to table
  output$sampling_rates <- renderDT({
    trk_all() |> summarize_sampling_rate_many(cols='id') |>
      datatable() |>
      formatRound(columns=c('min','q1','median','mean','q3','max','sd'), digits=2)
  })

  # Leaflet map with locations, home ranges, and disturbances
  output$map1 <- renderLeaflet({
    if (input$getButton) {
      years <- unique(gps_csv()$year)
      caribou_pal <- colorFactor(topo.colors(25), gps_csv()$id)
      cols <- col_yrs6[1:length(years)]
      year_pal <- colorNumeric(palette=cols, domain=years)
      m <- leaflet(options = leafletOptions(attributionControl=FALSE)) |>
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
        addProviderTiles("Esri.WorldGrayCanvas", group="Esri.WorldGrayCanvas") |>
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
        groups <- groups2 <- NULL
        trk_all <- mutate(trk_all(), year=as.double(year))
        for (i in sort(unique(trk_all$id))) {
          id1 <- trk_all |> filter(id==i & (year>=input$daterange[1] & year<=input$daterange[2]))
          id1 <- id1 |> mutate(first_obs = c(12, rep(2,nrow(id1)-1)))
          groups <- c(groups, paste0("ID ",i), paste0("TRK ",i))
          m <- m |> addCircles(data=id1, ~x_, ~y_, fill=T, stroke=T, weight=id1$first_obs, color=~year_pal(year), 
              fillColor=~year_pal(year), fillOpacity=1, group=paste0("ID ",i), popup=id1$t_) |>
            addPolylines(data=id1, lng=~x_, lat=~y_, color="orange", weight=2, group=paste0("TRK ",i))
        }
        m <- m |> 
          addLegend("topleft", colors=cols, labels=years, title="Year") |>
          addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) |>
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldGrayCanvas"),
            overlayGroups = c(groups),
            options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(groups[2:length(groups)]))
      m
    }
  })

}

shinyApp(ui, server)
