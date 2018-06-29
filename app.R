list.of.packages <-
  c(
    "shiny",
    "shinydashboard",
    "DT",
    "ggplot2",
    "data.table",
    "plotly",
    "reshape",
    "reshape2",
    "pbapply"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(data.table)
library(plotly)
library(reshape2)
library(reshape)
library(pbapply)

jan2017Data <- fread(file = "jan2017.csv", sep = ",", header = TRUE)

# jan2017Data <<-jan2017Data[((jan2017Data$ORIGIN_AIRPORT_ID != "13930" &
#                                 jan2017Data$ORIGIN_AIRPORT_ID != "13232") & (jan2017Data$DEST_AIRPORT_ID != "13930" &
#                                                                                jan2017Data$DEST_AIRPORT_ID != "13232")), ]

#jan2017Data <<- jan2017Data[(jan2017Data$CARRIER == "OO"),]

feb2017Data <- fread(file = "feb2017.csv", sep = ",", header = TRUE)

mar2017Data <- fread(file = "mar2017.csv", sep = ",", header = TRUE)

apr2017Data <- fread(file = "apr2017.csv", sep = ",", header = TRUE)

may2017Data <- fread(file = "may2017.csv", sep = ",", header = TRUE)

jun2017Data <- fread(file = "jun2017.csv", sep = ",", header = TRUE)

jul2017Data <- fread(file = "jul2017.csv", sep = ",", header = TRUE)

aug2017Data <- fread(file = "aug2017.csv", sep = ",", header = TRUE)

sep2017Data <- fread(file = "sep2017.csv", sep = ",", header = TRUE)

oct2017Data <- fread(file = "oct2017.csv", sep = ",", header = TRUE)

nov2017Data <- fread(file = "nov2017.csv", sep = ",", header = TRUE)

dec2017Data <- fread(file = "dec2017.csv", sep = ",", header = TRUE)

airportID <- fread(file = "airportID.csv", sep = ",", header = TRUE)



Hours2 <-
  c(
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '10',
    '11',
    '12',
    '13',
    '14',
    '15',
    '16',
    '17',
    '18',
    '19',
    '20',
    '21',
    '22',
    '23'
  )


dataFiles <-
  list(
    "January" = "jan2017.csv",
    "February" = "feb2017.csv",
    "March" = "mar2017.csv",
    "April" = "apr2017.csv",
    "May" = "may2017.csv",
    "June" = "jun2017.csv",
    "July" = "jul2017.csv",
    "August" = "aug2017.csv",
    "September" = "sep2017.csv",
    "October" = "oct2017.csv",
    "November" = "nov2017.csv",
    "December" = "dec2017.csv"
  )

allData <-
  list(
    "January" = jan2017Data,
    "February" = feb2017Data,
    "March" = mar2017Data,
    "April" = apr2017Data,
    "May" = may2017Data,
    "June" = jun2017Data,
    "July" = jul2017Data,
    "August" = aug2017Data,
    "September" = sep2017Data,
    "October" = oct2017Data,
    "November" = nov2017Data,
    "December" = dec2017Data
  )

allMonthsData <-
  list(
    jan2017Data,
    feb2017Data,
    mar2017Data,
    apr2017Data,
    may2017Data,
    jun2017Data,
    jul2017Data,
    aug2017Data,
    sep2017Data,
    oct2017Data,
    nov2017Data,
    dec2017Data
  )

monthsChoice = c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

Days <- c("Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday")

mergedMonths <- merge_all(allMonthsData)

airlineChoice <- unique(mergedMonths$CARRIER)

delayList <-
  c("Carrier", "Weather", "NAS", "Security", "Late Aircraft")

ui <- shinyUI(navbarPage(
  "Flight Data",
  tabPanel("C Stuff", mainPanel(
    tabsetPanel(
      tabPanel(
        "Arrival and Departure",
        sidebarPanel(
          selectInput(
            "airport",
            "Airport",
            choices = c("O'Hare", "Midway"),
            selected = "O,Hare"
          ),
          selectInput("month", "Month", choices = monthsChoice, selected = "January"),
          selectInput(
            "time",
            "Time",
            choices = c("Total", "Day of Week", "Hour of Day"),
            selected = "Total"
          ),
          uiOutput("hourSelection"),
          
          actionButton("button", "Apply")
        ),
        
        fluidRow(
          box(
            title = "Arrival and Departure Table",
            solidHeader = TRUE,
            status = "primary",
            width = 4,
            dataTableOutput("ADTable")
          )
          ,
          box(
            title = "Arrival and Departure Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("ADChart")
          )
        )
        
      ),
      tabPanel(
        "Delay",
        sidebarPanel(
          selectInput(
            "delayAirport",
            "Airport",
            choices = c("O'Hare", "Midway"),
            selected = "O'Hare"
          ),
          selectInput(
            "delayMonth",
            "Month",
            choices = monthsChoice,
            selected = "January"
          ),
          selectInput(
            "delayClock",
            "Clock",
            choices = c("12 Hour", "24 Hour"),
            selected = "12 Hour"
          ),
          actionButton("delayButton", "Apply")
        ),
        fluidRow(
          box(
            title = "Delay Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("delayTable")
          )
        ),
        
        fluidRow(
          box(
            title = "Delay Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput ("delayGraph")
            
          )
        )
        
      ),
      tabPanel(
        "Most Common 15",
        sidebarPanel(
          selectInput(
            "commonAirport",
            "Airport",
            choices = c("O'Hare", "Midway"),
            selected = "O'Hare"
          ),
          selectInput(
            "commonMonth",
            "Month",
            choices = monthsChoice,
            selected = "January
            "
          ),
          actionButton("commonButton", "Apply")
          ),
        #Put stuff here
        fluidRow(
          box(
            title = "Most Popular Arrivals Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("commonTable")
          )
        ),
        fluidRow(
          box(
            title = "Most Popular Arrivals Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("FifteenArriveGraph")
          )
        )
        
      )
      
      )
  )),
  tabPanel("B Stuff", mainPanel(
    tabsetPanel(
      tabPanel(
        "Arrival and Departure",
        sidebarPanel(
          selectInput("compareADTypes", "Type", choices = c("Airlines", "Hour of Day")),
          uiOutput("compareAirlinesSelection"),
          
          actionButton("compareAirlinesButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Arrival and Departure Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("compareAirlines")
          )
        ),
        fluidRow(
          box(
            title = "Arrival and Departure Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("compareAirlinesGraph")
          )
        )
      ),
      
      
      tabPanel(
        "Most Common 15",
        sidebarPanel(
          selectInput("allMonthsCommon", "Type", choices = c("Arrival", "Destination")),
          actionButton("allMonthsCommonButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Arrival and Departure Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("allMonthsCommonTable")
          )
        ),
        fluidRow(
          box(
            title = "Arrival and Departure Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("allMonthsCommonGraph")
          )
        )
      ),
      
      
      tabPanel(
        "Delay",
        sidebarPanel(
          selectInput("allMonthsDelay", "Delays", choices = delayList),
          actionButton("allMonthsDelayButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Delay Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("allMonthsDelayTable")
          )
        ),
        fluidRow(
          box(
            title = "Delay Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("allMonthsDelayGraph")
          )
        )
      )
    )
  )),
  
  
  
  tabPanel("A Stuff", mainPanel(
    tabsetPanel(
      tabPanel(
        "Pick Top 50",
        sidebarPanel(
          selectInput("pickTop50", "Type", choices = c("Airlines", "Hour of Day")),
          uiOutput("pickTop50Selection"),
          
          actionButton("pickTop50Button", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Arrival and Departure Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("top50Table")
          )
        ),
        fluidRow(
          box(
            title = "Arrival and Departure Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("top50Graph")
          )
        )
      ),
      
      
      tabPanel(
        "Pick an Airline",
        sidebarPanel(
          selectInput("pickAnAirline", "Airlines", choices = airlineChoice),
          actionButton("pickAnAirlineButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Airline Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("pickAnAirlineTable")
          )
        ),
        fluidRow(
          box(
            title = "Airline Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("pickAnAirlineGraph")
          )
        )
      ),
      
      
      tabPanel(
        "Pick a Date or Day",
        sidebarPanel(
          selectInput("pickDDType", "Type", choices = c("Date", "Day")),
          uiOutput("pickDDSelection"),
          actionButton("pickDDButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "AD and Delay Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("pickDDArrDepTable")
          )
        ),
        
        fluidRow(
          box(
            title = "AD Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("pickDDArrDepGraph")
          )
        ),
        fluidRow(
          box(
            title = "Delay Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("pickDDDelayGraph")
          )
        )
        
      ),
      
      
      tabPanel(
        "Pick a Delay",
        sidebarPanel(
          selectInput("pickDelay", "Delays", choices = delayList),
          
          actionButton("pickDelayButton", "Apply")
        )
        ,
        fluidRow(
          box(
            title = "Occurrence Table",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            dataTableOutput("pickDelayTable")
          )
        ),
        
        fluidRow(
          box(
            title = "Delay Occurrence Graph",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("pickDelayGraph")
          )
        )
        
      )
    )
  )),
  tabPanel("About", uiOutput("url"))
  
  
  
))

server <- function(input, output, session) {
  button <- eventReactive(input$button,
                          {
                            monthData <<-
                              allData[[input$month]]
                            airport <<- input$airport
                            timeChoice <<- input$time
                            clock <<- input$clock
                            
                          })
  
  output$ADTable <- renderDataTable({
    datatable({
      button()
      
      if (timeChoice == "Total") {
        carriers <- c(unique(monthData$CARRIER))
        airlines <- data.frame(carriers)
        
        airlines$arrival <- 0
        airlines$depart <- 0
        rownames(airlines) <- carriers
        
        f <- function(x, output) {
          C <- x["CARRIER"]
          origin <- x["ORIGIN_AIRPORT_ID"]
          dest <- x["DEST_AIRPORT_ID"]
          
          originName <- x["ORIGIN_CITY_NAME"]
          destName <- x["DEST_CITY_NAME"]
          
          if (airport == "O'Hare")
          {
            if (origin == "13930" & !grepl("*, IL", destName))
            {
              airlines[C, "depart"] <<- airlines[C, "depart"] + 1
            }
            else if (dest == "13930" & !grepl("*, IL", originName))
            {
              airlines[C, "arrival"] <<- airlines[C, "arrival"] + 1
            }
          }
          
          if (airport == "Midway")
          {
            if (origin == "13232")
            {
              airlines[C, "depart"] <<- airlines[C, "depart"] + 1
            }
            else if (dest == "13232")
            {
              airlines[C, "arrival"] <<- airlines[C, "arrival"] + 1
            }
          }
        }
        
        apply(monthData, 1, f)
        
        airlines <- airlines[order(airlines$carriers), ]
        
        globalAirlines <<- airlines
        airlines
      }
      
      else if (timeChoice == "Day of Week") {
        flightWeek <- data.frame(Days)
        flightWeek$Arrival <- 0
        flightWeek$Departure <- 0
        rownames(flightWeek) <- Days
        
        if (airport == "O'Hare") {
          for (x in Days) {
            flightDate <- monthData$FL_DATE
            flightDay <- weekdays(as.Date(flightDate))
            origin <- monthData$ORIGIN_AIRPORT_ID
            dest <- monthData$DEST_AIRPORT_ID
            flightWeek[x, "Departure"] <-
              sum(flightDay == x & origin == "13930")
            flightWeek[x, "Arrival"] <-
              sum(flightDay == x & dest == "13930")
            
          }
        }
        
        if (airport == "Midway") {
          for (x in Days) {
            flightDate <- flightWeek["FL_DATE"]
            flightDay <- weekdays(as.Date(flightDate))
            origin <- x["ORIGIN_AIRPORT_ID"]
            dest <- x["DEST_AIRPORT_ID"]
            flightWeek[flightDay, "Departure"] <-
              sum(flightDay == x & origin == "13930")
            flightWeek[flightDay, "Arrival"] <-
              sum(flightDay == x & dest == "13930")
            
          }
        }
        
        globalFlightWeek <<- flightWeek
        flightWeek
      }
      
      
      else if (timeChoice == "Hour of Day") {
        times = c(0,
                  1,
                  2,
                  3,
                  4,
                  5,
                  6,
                  7,
                  8,
                  9,
                  10,
                  11,
                  12,
                  13,
                  14,
                  15,
                  16,
                  17,
                  18,
                  19,
                  20,
                  21,
                  22,
                  23)
        
        if (clock == "24 Hour") {
          times = c(0,
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    18,
                    19,
                    20,
                    21,
                    22,
                    23)
        } else {
          times = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
        }
        
        hourId <-
          data.frame(hour(strptime(monthData$DEP_TIME, "%H%M")), monthData$ORIGIN_AIRPORT_ID)
        colnames(hourId) <- c("hour", "id")
        hourId <-
          subset(hourId, hourId$id == 13930 | hourId$id == 13232)
        
        #Count occurances of each hour in hourId
        myList <- list()
        for (i in 0:23) {
          myList[i + 1] = sum(hourId$hour == i, na.rm = TRUE)
        }
        hourDataCnt <- data.frame(unlist(myList))
        colnames(hourDataCnt) <- c("occurances")
        dt <- data.frame(times, hourDataCnt)
        
        globalHourFlightWeek <<- dt
        dt
      }
      
    },
    options = list(
      searching = FALSE,
      pageLength = 11,
      lengthChange = FALSE
    ), rownames = FALSE)
  })
  
  output$ADChart <- renderPlotly({
    if (input$button > 0) {
      if (timeChoice == "Total") {
        p <-
          plot_ly(
            globalAirlines,
            x = ~ globalAirlines$carriers,
            y = ~ globalAirlines$arrival,
            type = 'bar',
            name = 'arrival'
          ) %>%
          add_trace(y = ~ globalAirlines$depart, name = 'depart') %>%
          layout(
            yaxis = list(title = 'Count'),
            xaxis = list(title = 'Airline'),
            barmode = 'group'
          )
        p
      }
      
      else if (timeChoice == "Day of Week") {
        globalFlightWeek$Days <-
          factor(globalFlightWeek$Days, levels = Days)
        b <-
          plot_ly(
            globalFlightWeek,
            x = ~ globalFlightWeek$Days,
            y = ~ globalFlightWeek$Arrival,
            type = 'bar',
            name = 'arrival'
          ) %>%
          add_trace(y = ~ globalFlightWeek$Departure,
                    name = 'Departure') %>%
          layout(
            yaxis = list(title = 'Count'),
            xaxis = list(title = 'Day'),
            barmode = 'group'
          )
        b
      }
      
      else if (timeChoice == "Hour of Day") {
        if (clock == "12 Hour") {
          am <- globalHourFlightWeek[1:12, 1:2]
          am$times <-
            factor(am$times, levels = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
          pm <- globalHourFlightWeek[13:24, 1:2]
          plotly::plot_ly(
            am,
            x = am$times,
            y = am$occurances,
            name = 'AM' ,
            type = "scatter",
            mode = "lines"
          ) %>%
            add_trace(y = pm$occurances,
                      name = 'PM',
                      mode = 'lines')
        } else {
          plotly::plot_ly(
            globalHourFlightWeek,
            x = globalHourFlightWeek$times,
            y = globalHourFlightWeek$occurances,
            type = "scatter",
            mode = "lines"
          )
        }
      }
    } else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  delayButton <- eventReactive(input$delayButton, {
    delayMonthData <<-
      allData[[input$delayMonth]]
    
    delayAirport <<- input$delayAirport
    timeFmt24 <<- input$delayClock
  })
  
  output$delayTable <- renderDataTable({
    datatable({
      delayButton()
      
      if(delayAirport == "O'Hare") {
      if (timeFmt24 == "24 Hour") {
        times = c(0,
                  1,
                  2,
                  3,
                  4,
                  5,
                  6,
                  7,
                  8,
                  9,
                  10,
                  11,
                  12,
                  13,
                  14,
                  15,
                  16,
                  17,
                  18,
                  19,
                  20,
                  21,
                  22,
                  23)
      } else {
        times = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      }
      hourId <-
        data.frame(
          hour(strptime(delayMonthData$DEP_TIME, "%H%M")),
          delayMonthData$ORIGIN_AIRPORT_ID,
          delayMonthData$CARRIER_DELAY
        )
      colnames(hourId) <- c("hour", "id", "delays")
      hourId <-
        subset(hourId, hourId$id == 13930)
      hourId <- subset(hourId, !is.na(hourId$delays))
      
      if (FALSE) {
        hourId <- hourId$hour %% 12 + 1
      }
      
      #Count occurances of each hour in hourId
      myList <- list()
      for (i in 0:23) {
        myList[i + 1] = sum(hourId$hour == i, na.rm = TRUE)
      }
      hourDataCnt <- data.frame(unlist(myList))
      colnames(hourDataCnt) <- c("occurances")
      dt <- data.frame(times, hourDataCnt)
      
      globalDT <<- dt
      
      dt
      }
      
      else if(delayAirport == "Midway") {
        if (timeFmt24 == "24 Hour") {
          times = c(0,
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    18,
                    19,
                    20,
                    21,
                    22,
                    23)
        } else {
          times = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
        }
        hourId <-
          data.frame(
            hour(strptime(delayMonthData$DEP_TIME, "%H%M")),
            delayMonthData$ORIGIN_AIRPORT_ID,
            delayMonthData$CARRIER_DELAY
          )
        colnames(hourId) <- c("hour", "id", "delays")
        hourId <-
          subset(hourId, hourId$id == 13232)
        hourId <- subset(hourId, !is.na(hourId$delays))
        
        if (FALSE) {
          hourId <- hourId$hour %% 12 + 1
        }
        
        #Count occurances of each hour in hourId
        myList <- list()
        for (i in 0:23) {
          myList[i + 1] = sum(hourId$hour == i, na.rm = TRUE)
        }
        hourDataCnt <- data.frame(unlist(myList))
        colnames(hourDataCnt) <- c("occurances")
        dt <- data.frame(times, hourDataCnt)
        
        globalDT <<- dt
        
        dt
      }
      
      
    })
  })
  
  output$delayGraph <- renderPlotly({
    if (input$delayButton > 0) {
      if (timeFmt24 != "24 Hour") {
        am <- globalDT[1:12, 1:2]
        am$times <-
          factor(am$times, levels = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        pm <- globalDT[13:24, 1:2]
        plotly::plot_ly(
          am,
          x = am$times,
          y = am$occurances,
          name = 'AM' ,
          type = "scatter",
          mode = "lines"
        ) %>%
          add_trace(y = pm$occurances,
                    name = 'PM',
                    mode = 'lines')
      } else {
        plotly::plot_ly(
          globalDT,
          x = globalDT$times,
          y = globalDT$occurances,
          type = "scatter",
          mode = "lines"
        )
      }
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  commonButton <- eventReactive(input$commonButton, {
    commonMonthData <<-
      allData[[input$commonMonth]]
    commonAirport <<- input$commonAirport
    
  })
  
  output$commonTable <-
    renderDataTable({
      datatable({
        commonButton()
        
        if (commonAirport == "O'Hare") {
          dt <-
            subset(commonMonthData,
                   commonMonthData$DEST_AIRPORT_ID == "13930")
          tempT <-
            as.data.frame(sort(table(dt$ORIGIN_AIRPORT_ID), decreasing = TRUE)[1:15])
          df <-
            data.frame(id = levels(tempT$Var1),
                       numArrivals = tempT$Freq)
          ty <-
            data.frame(airportID[match(df[["id"]], airportID[["Code"]]), "Description"])
          colnames(ty) <- c("Name")
          table <- data.frame(ty$Name, df$numArrivals)
          colnames(table) <- c("Name", "numArrivals")
          
          globalCommonTable <<- table
          
          table
        }
        
        else if (commonAirport == "Midway") {
          dt <-
            subset(commonMonthData,
                   commonMonthData$DEST_AIRPORT_ID == "13232")
          tempT <-
            as.data.frame(sort(table(dt$ORIGIN_AIRPORT_ID), decreasing = TRUE)[1:15])
          df <-
            data.frame(id = levels(tempT$Var1),
                       numArrivals = tempT$Freq)
          ty <-
            data.frame(airportID[match(df[["id"]], airportID[["Code"]]), "Description"])
          colnames(ty) <- c("Name")
          table <- data.frame(ty$Name, df$numArrivals)
          colnames(table) <- c("Name", "numArrivals")
          
          globalCommonTable <<- table
          
          table
        }
        
      })
    })
  
  
  output$FifteenArriveGraph <- renderPlotly({
    if (input$commonButton > 0) {
      plotly::plot_ly(
        globalCommonTable,
        x = globalCommonTable$Name,
        y = globalCommonTable$numArrivals,
        type = "bar",
        color = I("rgba(0,92,124,1)")
      )
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  output$hourSelection <-
    renderUI({
      if (input$time == "Hour of Day") {
        selectInput("clock",
                    "Clock",
                    choices = c("12 Hour", "24 Hour"))
      }
    })
  
  output$compareAirlinesSelection <- renderUI({
    if (input$compareADTypes == "Airlines") {
      selectInput("compareAirlines", "Airlines", choices = airlineChoice)
    }
    else if (input$compareADTypes == "Hour of Day") {
      selectInput("compareADHours", "Hours", choices = Hours2)
    }
  })
  
  compareAirlinesButton <-
    eventReactive(input$compareAirlinesButton, {
      allAirlinesData <<- mergedMonths
      pickAirline <<- input$compareAirlines
      airlinesAirport <<- input$compareAirlinesAirport
      pickType <<- input$compareADTypes
      pickADHours <<- input$compareADHours
    })
  
  output$compareAirlines <- renderDataTable({
    compareAirlinesButton()
    
    if (pickType == "Airlines") {
      subsetAirline <-
        subset(allAirlinesData, allAirlinesData$CARRIER == pickAirline)
      
      compareAirlinesTable <- data.frame(monthsChoice)
      compareAirlinesTable$Arrival <- 0
      compareAirlinesTable$Departure <- 0
      rownames(compareAirlinesTable) <- monthsChoice
      
      for (x in monthsChoice) {
        compareAirlinesTable[x, "Departure"] <-
          sum(grepl("*, IL", subsetAirline$ORIGIN_CITY_NAME) &
                months(as.Date(subsetAirline$FL_DATE)) == x)
        compareAirlinesTable[x, "Arrival"] <-
          sum(grepl("*, IL", subsetAirline$DEST_CITY_NAME) &
                months(as.Date(subsetAirline$FL_DATE)) == x)
      }
      
      #compareAirlinesTable <- compareAirlinesTable[order(compareAirlinesTable$monthChoice), ]
      
      globalCompareAirlinesTable <<- compareAirlinesTable
      compareAirlinesTable
    }
    else if (pickType == "Hour of Day") {
      times = c(0,
                1,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                11,
                12,
                13,
                14,
                15,
                16,
                17,
                18,
                19,
                20,
                21,
                22,
                23)
      
      
      hourId <-
        data.frame(mergedMonths$FL_DATE,
                   hour(strptime(mergedMonths$DEP_TIME, "%H%M")),
                   mergedMonths$ORIGIN_AIRPORT_ID)
      colnames(hourId) <- c("date", "hour", "id")
      hourId <-
        subset(hourId, hourId$id == 13930 | hourId$id == 13232)
      
      #Count occurances of each hour in hourId
      myList <- list()
      for (i in monthsChoice) {
        monthDate <- months(as.Date(hourId$date))
        myList[i] = sum(hourId$hour == as.numeric(as.character(pickADHours)) &
                          monthDate == i,
                        na.rm = TRUE)
      }
      hourDataCnt <- data.frame(unlist(myList))
      colnames(hourDataCnt) <- c("occurances")
      dt <- data.frame(monthsChoice, hourDataCnt)
      
      globalCompareHoursTable <<- dt
      dt
    }
    
  })
  
  output$compareAirlinesGraph <- renderPlotly({
    if (input$compareAirlinesButton > 0) {
      if (pickType == "Airlines") {
        globalCompareAirlinesTable$monthsChoice <-
          factor(globalCompareAirlinesTable$monthsChoice, levels = monthsChoice)
        p <-
          plot_ly(
            globalCompareAirlinesTable,
            x = globalCompareAirlinesTable$monthsChoice,
            y = ~ globalCompareAirlinesTable$Arrival,
            name = "Arrival",
            type = 'bar'
          ) %>%
          add_trace(y = globalCompareAirlinesTable$Departure,
                    name = "Departure")
        p
      }
      else if (pickType == "Hour of Day") {
        globalCompareHoursTable$monthsChoice <-
          factor(globalCompareHoursTable$monthsChoice, levels = monthsChoice)
        p <-
          plot_ly(
            globalCompareHoursTable,
            x = globalCompareHoursTable$monthsChoice,
            y = ~ globalCompareHoursTable$occurances,
            name = "Arrival",
            type = 'scatter',
            line = 'line'
          )
        p
      }
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  
  allMonthsCommonButton <-
    eventReactive(input$allMonthsCommonButton, {
      allMonthsCommonType <<- input$allMonthsCommon
    })
  
  output$allMonthsCommonTable <- renderDataTable({
    allMonthsCommonButton()
    
    if(allMonthsCommonType == "Arrival"){
    tempT <-
      as.data.frame(sort(table(mergedMonths$ORIGIN_AIRPORT_ID), decreasing = TRUE)[1:15])
    df <-
      data.frame(id = levels(tempT$Var1),
                 numArrivals = tempT$Freq)
    ty <-
      data.frame(airportID[match(df[["id"]], airportID[["Code"]]), "Description"])
    colnames(ty) <- c("Name")
    table <- data.frame(ty$Name, df$numArrivals)
    colnames(table) <- c("Name", "numArrivals")
    
    globalCommonTable <<- table
    
    table
    }
    
    else if(allMonthsCommonType == "Destination"){
    tempT <-
      as.data.frame(sort(table(mergedMonths$DEST_AIRPORT_ID), decreasing = TRUE)[1:15])
    df <-
      data.frame(id = levels(tempT$Var1),
                 numArrivals = tempT$Freq)
    ty <-
      data.frame(airportID[match(df[["id"]], airportID[["Code"]]), "Description"])
    colnames(ty) <- c("Name")
    table <- data.frame(ty$Name, df$numArrivals)
    colnames(table) <- c("Name", "numArrivals")
    
    globalCommonTable <<- table
    
    table
    }
    
  })
  
  output$pickDDSelection <- renderUI({
    if (input$pickDDType == "Date") {
      textInput("inputDate", "YYYY-MM-DD", value = "")
    }
    else if (input$pickDDType == "Day") {
      selectInput("inputDay", "Days", choices = Days)
    }
    
  })
  
  
  pickDDButton <- eventReactive(input$pickDDButton,
                                {
                                  inputDDType <<- input$pickDDType
                                  inputDate <<- input$inputDate
                                  inputDay <<- input$inputDay
                                })
  
  output$pickDDArrDepTable <- renderDataTable({
    pickDDButton()
    
    allMonths <- mergedMonths
    if (inputDDType == "Date") {
      subsetMonths <- subset(allMonths, allMonths$FL_DATE == inputDate)
      
      hourId <-
        data.frame(
          hour(strptime(subsetMonths$DEP_TIME, "%H%M")),
          hour(strptime(subsetMonths$ARR_TIME, "%H%M")),
          subsetMonths$ORIGIN_CITY_NAME,
          subsetMonths$DEST_CITY_NAME,
          subsetMonths$CARRIER_DELAY
        )
      colnames(hourId) <-
        c("depHour", "arrHour", "origin", "dest", "carrDelay")
      
      dateData <- data.frame(Hours2)
      dateData$Arrival <- 0
      dateData$Departure <- 0
      dateData$Delays <- 0
      rownames(dateData) <- Hours2
      
      for (x in Hours2) {
        depHour <- hourId$depHour
        arrHour <- hourId$arrHour
        origin <- hourId$origin
        dest <- hourId$dest
        delay <- hourId$carrDelay
        dateData[x, "Departure"] <-
          sum(depHour == x & grepl("*, IL", origin), na.rm = TRUE)
        dateData[x, "Arrival"] <-
          sum(arrHour == x & grepl("*, IL", dest) , na.rm = TRUE)
        dateData[x, "Delays"] <-
          sum(delay >= 0 & (depHour == x | arrHour == x), na.rm = TRUE)
      }
      
      globalDateData <<- dateData
      dateData
      
    }
    
    else if (inputDDType == "Day") {
      subsetMonths <-
        subset(allMonths, weekdays(as.Date(allMonths$FL_DATE)) == inputDay)
      
      hourId <-
        data.frame(
          hour(strptime(subsetMonths$DEP_TIME, "%H%M")),
          hour(strptime(subsetMonths$ARR_TIME, "%H%M")),
          subsetMonths$ORIGIN_CITY_NAME,
          subsetMonths$DEST_CITY_NAME,
          subsetMonths$CARRIER_DELAY
        )
      colnames(hourId) <-
        c("depHour", "arrHour", "origin", "dest", "carrDelay")
      
      dateData <- data.frame(Hours2)
      dateData$Arrival <- 0
      dateData$Departure <- 0
      dateData$Delays <- 0
      rownames(dateData) <- Hours2
      
      for (x in Hours2) {
        depHour <- hourId$depHour
        arrHour <- hourId$arrHour
        origin <- hourId$origin
        dest <- hourId$dest
        delay <- hourId$carrDelay
        dateData[x, "Departure"] <-
          sum(depHour == x & grepl("*, IL", origin), na.rm = TRUE)
        dateData[x, "Arrival"] <-
          sum(arrHour == x & grepl("*, IL", dest) , na.rm = TRUE)
        dateData[x, "Delays"] <-
          sum(delay >= 0 & (depHour == x | arrHour == x), na.rm = TRUE)
      }
      
      globalDateData <<- dateData
      dateData
      
    }
    
  })
  
  output$pickDDArrDepGraph <- renderPlotly({
    if (input$pickDDButton > 0) {
      if (inputDDType == "Date") {
        globalDateData$Hours2 <-
          factor(globalDateData$Hours2, levels = Hours2)
        p <-
          plot_ly(
            globalDateData,
            x = ~ globalDateData$Hours2,
            y = ~ globalDateData$Arrival,
            name = "Arrival",
            type = 'scatter',
            mode = "lines"
          ) %>%
          add_trace(y = globalDateData$Departure,
                    name = "Departure")
        p
      }
      
      else if (inputDDType == "Day") {
        globalDateData$Hours2 <-
          factor(globalDateData$Hours2, levels = Hours2)
        p <-
          plot_ly(
            globalDateData,
            x = ~ globalDateData$Hours2,
            y = ~ globalDateData$Arrival,
            name = "Arrival",
            type = 'scatter',
            mode = "lines"
          ) %>%
          add_trace(y = globalDateData$Departure,
                    name = "Departure")
        p
      }
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  
  output$pickDDDelayGraph <- renderPlotly({
    if (input$pickDDButton > 0) {
      if (inputDDType == "Date") {
        globalDateData$Hours2 <-
          factor(globalDateData$Hours2, levels = Hours2)
        p <-
          plot_ly(
            globalDateData,
            x = ~ globalDateData$Hours2,
            y = ~ globalDateData$Delays,
            name = "Delays",
            type = 'scatter',
            mode = "lines"
          )
        p
      }
      
      else if (inputDDType == "Day") {
        globalDateData$Hours2 <-
          factor(globalDateData$Hours2, levels = Hours2)
        p <-
          plot_ly(
            globalDateData,
            x = ~ globalDateData$Hours2,
            y = ~ globalDateData$Delays,
            name = "Delays",
            type = 'scatter',
            mode = "lines"
          )
        p
      }
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  pickAnAirlineButton <- eventReactive(input$pickAnAirlineButton, {
    pickedAirline <<- input$pickAnAirline
  })
  output$pickAnAirlineTable <- renderDataTable({
    pickAnAirlineButton()
    allMonths <- mergedMonths
    
    subsetMonths <-
      subset(allMonths, allMonths$CARRIER == pickedAirline)
    
    
    hourId <-
      data.frame(
        hour(strptime(subsetMonths$DEP_TIME, "%H%M")),
        hour(strptime(subsetMonths$ARR_TIME, "%H%M")),
        subsetMonths$ORIGIN_CITY_NAME,
        subsetMonths$DEST_CITY_NAME,
        subsetMonths$CARRIER_DELAY
      )
    colnames(hourId) <-
      c("depHour", "arrHour", "origin", "dest", "carrDelay")
    
    
    airlineData <- data.frame(Hours2)
    airlineData$Arrival <- 0
    airlineData$Departure <- 0
    for (x in Hours2) {
      depHour <- hourId$depHour
      arrHour <- hourId$arrHour
      origin <- hourId$origin
      dest <- hourId$dest
      
      airlineData[x, "Departure"] <-
        sum(depHour == x & grepl("*, IL", origin), na.rm = TRUE)
      airlineData[x, "Arrival"] <-
        sum(arrHour == x & grepl("*, IL", dest) , na.rm = TRUE)
    }
    
    globalAirlineData <<- airlineData
    airlineData
  })
  
  
  output$pickAnAirlineGraph <- renderPlotly({
    if (input$pickAnAirlineButton > 0) {
      globalAirlineData$Hours2 <-
        factor(globalAirlineData$Hours2, levels = Hours2)
      p <-
        plot_ly(
          globalAirlineData,
          x = ~ globalAirlineData$Hours2,
          y = ~ globalAirlineData$Arrival,
          name = "Delays",
          type = 'scatter',
          mode = "lines"
        ) %>%
        add_trace(y = globalAirlineData$Departure,
                  name = "Departure")
      p
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
  })
  
  
  
  pickDelayButton <- eventReactive(input$pickDelayButton, {
    pickDelay <<- input$pickDelay
  })
  
  output$pickDelayTable <- renderDataTable({
    pickDelayButton()
    
    allMonths = mergedMonths
    
    subsetMonths <- subset(allMonths, allMonths$CARRIER_DELAY >= 0)
    
    
    hourId <-
      data.frame(
        hour(strptime(subsetMonths$DEP_TIME, "%H%M")),
        hour(strptime(subsetMonths$ARR_TIME, "%H%M")),
        subsetMonths$CARRIER_DELAY,
        subsetMonths$WEATHER_DELAY,
        subsetMonths$NAS_DELAY,
        subsetMonths$SECURITY_DELAY,
        subsetMonths$LATE_AIRCRAFT_DELAY
      )
    colnames(hourId) <-
      c("depHour",
        "arrHour",
        "carrier",
        "weather",
        "nas",
        "security",
        "lateAircraft")
    
    
    delayData <- data.frame(Hours2)
    delayData$Occurrence <- 0
    
    for (x in Hours2) {
      depHour <- hourId$depHour
      carrier <- hourId$carrier
      weather <- hourId$weather
      nas <- hourId$nas
      security <- hourId$security
      lateAircraft <- hourId$lateAircraft
      
      if (pickDelay == "Carrier")
        delayData[x, "Occurrence"] <-
        sum(depHour == x & carrier > 1, na.rm = TRUE)
      
      if (pickDelay == "Weather")
        delayData[x, "Occurrence"] <-
        sum(depHour == x & weather > 1, na.rm = TRUE)
      
      if (pickDelay == "NAS")
        delayData[x, "Occurrence"] <-
        sum(depHour == x & nas > 1, na.rm = TRUE)
      
      if (pickDelay == "Security")
        delayData[x, "Occurrence"] <-
        sum(depHour == x & security > 1, na.rm = TRUE)
      
      if (pickDelay == "Late Aircraft")
        delayData[x, "Occurrence"] <-
        sum(depHour == x & lateAircraft > 1, na.rm = TRUE)
    }
    
    globalDelayData <<- delayData
    delayData
    
  })
  
  
  output$pickDelayGraph <- renderPlotly({
    if (input$pickDelayButton > 0) {
      globalDelayData$Hours2 <-
        factor(globalDelayData$Hours2, levels = Hours2)
      p <-
        plot_ly(
          globalDelayData,
          x = ~ globalDelayData$Hours2,
          y = ~ globalDelayData$Occurrence,
          name = "Delays",
          type = 'scatter',
          mode = "lines"
        )
      p
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
    
  })
  
  allMonthsDelayButton <- eventReactive(input$allMonthsDelayButton, {
    monthsDelay <<- input$allMonthsDelay
  })
  
  output$allMonthsDelayTable <- renderDataTable({
    
    allMonthsDelayButton()
    
    allMonths = mergedMonths
    
    subsetMonths <- subset(allMonths, allMonths$CARRIER_DELAY >= 0)
    
    
    monthId <-
      data.frame(
        months(as.Date(subsetMonths$FL_DATE)),
        subsetMonths$CARRIER_DELAY,
        subsetMonths$WEATHER_DELAY,
        subsetMonths$NAS_DELAY,
        subsetMonths$SECURITY_DELAY,
        subsetMonths$LATE_AIRCRAFT_DELAY
      )
    colnames(monthId) <-
      c("month",
        "carrier",
        "weather",
        "nas",
        "security",
        "lateAircraft")
    
    
    delayData <- data.frame(monthsChoice)
    delayData$Occurrence <- 0
    rownames(delayData) <- monthsChoice
    
    for (x in monthsChoice) {
      delayMonth <- monthId$month
      carrier <- monthId$carrier
      weather <- monthId$weather
      nas <- monthId$nas
      security <- monthId$security
      lateAircraft <- monthId$lateAircraft
      
      if (monthsDelay == "Carrier")
        delayData[x, "Occurrence"] <-
        sum(delayMonth == x & carrier > 1, na.rm = TRUE)
      
      if (monthsDelay == "Weather")
        delayData[x, "Occurrence"] <-
        sum(delayMonth == x & weather > 1, na.rm = TRUE)
      
      if (monthsDelay == "NAS")
        delayData[x, "Occurrence"] <-
        sum(delayMonth == x & nas > 1, na.rm = TRUE)
      
      if (monthsDelay == "Security")
        delayData[x, "Occurrence"] <-
        sum(delayMonth == x & security > 1, na.rm = TRUE)
      
      if (monthsDelay == "Late Aircraft")
        delayData[x, "Occurrence"] <-
        sum(delayMonth == x & lateAircraft > 1, na.rm = TRUE)
    }
    
    globalMonthDelayData <<- delayData
    delayData
    
  })
  
  
  output$allMonthsDelayGraph <- renderPlotly({
    
    
    if (input$allMonthsDelayButton > 0) {
      globalMonthDelayData$monthsChoice <-
        factor(globalMonthDelayData$monthsChoice, levels = monthsChoice)
      p <-
        plot_ly(
          globalMonthDelayData,
          x = ~ globalMonthDelayData$monthsChoice,
          y = ~ globalMonthDelayData$Occurrence,
          name = "Delays",
          type = 'scatter',
          mode = "lines"
        )
      p
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
    
    
  })
  
  output$url <- renderUI({
    url <- a("http://jwalle9.people.uic.edu/evl/project2.php", href = "http://jwalle9.people.uic.edu/evl/project2.php") 
    tagList("Created By: Joseph Galante, Vincent Pham, and Jacob Waller")
    tagList("Our Project Page: ", url)
  })
  
  output$allMonthsCommonGraph <- renderPlotly({
    
    if (input$allMonthsCommonButton > 0) {

      p <-
        plot_ly(
          globalCommonTable,
          x = ~ globalCommonTable$Name,
          y = ~ globalCommonTable$numArrivals,
          name = "Arrivals",
          type = 'bar'
    
        )
      p
    }
    else{
      plot_ly(type = "scatter", mode = "lines")
    }
    
  })
  
}
shinyApp(ui = ui, server = server)