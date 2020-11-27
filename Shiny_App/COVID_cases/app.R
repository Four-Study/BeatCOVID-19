#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tigris)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 cases count in Virginia"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "region",
                        label = "Select a region",
                        choices = c("All",
                                    "Alexandria", "Alleghany", 
                                    "Arlington", "Central Shenandoah",
                                    "Central Virginia", "Chesapeake",
                                    "Chesterfield", "Chickahominy",
                                    "Crater", "Cumberland Plateau",
                                    "Eastern Shore","Fairfax",
                                    "Hampton","Henrico",
                                    "Lenowisco", "Lord Fairfax",
                                    "Loudoun","Mount Rogers",
                                    "New River", "Norfolk",
                                    "Peninsula", "Piedmont",
                                    "Pittsylvania-Danville", "Portsmouth",
                                    "Prince William", "Rappahannock",
                                    "Rappahannock Rapidan", "Richmond",
                                    "Roanoke", "Southside",
                                    "Thomas Jefferson", "Three Rivers",
                                    "Virginia Beach", "West Piedmont",
                                    "Western Tidewater")),
            selectInput(inputId = "age",
                        label = "Select an age range",
                        choices = c("All", 
                                    "0-9", "10-19", 
                                    "20-29", "30-39",
                                    "40-49", "50-59",
                                    "60-69", "70-79",
                                    "80+", "Missing")),
            dateRangeInput("daterange", "Date range:",
                           start = "2020-03-29",
                           end   = "2020-11-23",
                           min    = "2020-03-29",
                           max    = "2020-11-23",
                           format = "mm/dd/yyyy")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Trend Plot", plotOutput("tsplot")), 
                tabPanel("Map", plotOutput("map")), 
                tabPanel("Bar Plot", plotOutput("bar1")),
                tabPanel("Data Table", dataTableOutput("table"))
            ),
            h6("Developed by Yixin Chen, Youhui Ye and Zhiyuan Du.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dat <- read.csv("VDH-COVID-19-PublicUseDataset-Cases_By-Age-Group.csv")
    dat$dates <- as.Date(dat$Report.Date, "%m/%d/%Y")
    output$tsplot <- renderPlot({
        # filter records by features
        if (input$region == "All"){
            dist_filter <- TRUE
        } else {
            dist_filter <- dat$Health.District == input$region
        }
        if (input$age == "All"){
            age_filter <- TRUE
        } else {
            age_filter <- dat$Age.Group == input$age
        }
        date_filter <- (dat$dates >= input$daterange[1]) &
            (dat$dates <= input$daterange[2])
        indices <- dist_filter & age_filter & date_filter
        selected <- dat[indices, ]
        # time series data
        ts <- selected %>% 
            group_by(dates) %>% 
            summarise(num = sum(Number.of.Cases), .groups = 'drop')
        if(nrow(ts) <= 5) 
            ggplot() + annotate("text", x = 4, y = 25, size = 10, label = "No enough data")        
        else {
            p <- ggplot(ts, aes(x=dates, y=num)) +
                geom_line(size = 1, color = "red") + 
                xlab("Date") + 
                ylab("Number of Cases") + 
                theme(axis.title.y = element_text(size = rel(1.5)),
                      axis.title.x = element_text(size = rel(1.5)),
                      axis.text.y = element_text(size = rel(1.2)),
                      axis.text.x = element_text(size = rel(1.2)),
                      aspect.ratio=3/4)
            p
        }
        
    })
    output$bar1 <- renderPlot({
        # filter records by features
        if (input$region == "All"){
            dist_filter <- TRUE
        } else {
            dist_filter <- dat$Health.District == input$region
        }
        date_filter <- (dat$dates >= input$daterange[1]) &
            (dat$dates <= input$daterange[2])
        indices <- dist_filter & date_filter
        selected <- dat[indices, ]
        # time series data
        grouped_by_age <- selected %>% 
            group_by(Age.Group) %>% 
            summarise(num = sum(Number.of.Cases), .groups = 'drop')
        p1 <- ggplot(grouped_by_age, aes(y=num, x=Age.Group)) + 
            geom_bar(stat="identity", fill="steelblue") + 
            geom_text(aes(label=num), vjust=1.6, color="white", size=3.5)+
            theme_minimal() + ylab("Number of Cases") + 
            theme(axis.title.y = element_text(size = rel(1.5)),
                  axis.title.x = element_text(size = rel(1.5)),
                  axis.text.y = element_text(size = rel(1.2)),
                  axis.text.x = element_text(size = rel(1.2)),
                  aspect.ratio=3/5)
        p1
    })
    
    output$map <- renderPlot({
        county_HD <- read_excel("County_HealthDistrict.xlsx")
        county_HD <- county_HD[,-4] #delete region name
        #sort by ID(CountyFIPS)
        county_HD <- county_HD[order(county_HD$CountyFIPS),] 
        
        #dataset used to draw map
        va <- counties("Virginia", cb = TRUE)
        #keep GEOID(the same as CountyFIPS), Name and geometry
        va <- va[,c(5,6,10)]
        #sort by GEOID so that "va" and "county_HD" have the same 1st column
        va <- va[order(va$GEOID),]
        #add HealthDistric column to "va"
        va$HealthDistrict<-county_HD$HealthDistrict
        #sort by Health district so that we can join with "dis.cases.tot"
        va <- va[order(va$HealthDistrict),]
        
        # filter records by side bar
        if (input$age == "All"){
            age_filter <- TRUE
        } else {
            age_filter <- dat$Age.Group == input$age
        }
        date_filter <- (dat$dates >= input$daterange[1]) &
            (dat$dates <= input$daterange[2])
        indices <- age_filter & date_filter
        selected <- dat[indices, ]
        
        grouped_by_region <- selected %>% 
            group_by(Health.District) %>% 
            summarise(Cases = sum(Number.of.Cases), .groups = 'drop') %>%
            slice(-1)
        grouped_by_region <- dat %>% 
            group_by(Health.District) %>% 
            summarise(.groups = 'drop') %>% slice(-1) %>% 
            left_join(grouped_by_region, by = "Health.District")
        
        grouped_by_region$HealthDistrict <- unique(va$HealthDistrict)
        join <- va %>% left_join(grouped_by_region,by="HealthDistrict")
        
        ggplot(data = join) +
            geom_sf(aes(fill = Cases),lwd=0) + 
            scale_fill_gradient(low = 'white',
                                high = 'red', 
                                label = scales::comma,
                                trans = "log10") + 
            theme(axis.text.y = element_text(size = rel(1.2)),
                  axis.text.x = element_text(size = rel(1.2)))
        
    })
    
    output$table <- renderDataTable({
        if (input$region == "All"){
            dist_filter <- TRUE
        } else {
            dist_filter <- dat$Health.District == input$region
        }
        if (input$age == "All"){
            age_filter <- TRUE
        } else {
            age_filter <- dat$Age.Group == input$age
        }
        date_filter <- (dat$dates >= input$daterange[1]) &
            (dat$dates <= input$daterange[2])
        indices <- dist_filter & age_filter & date_filter
        selected <- dat[indices, ]},
        
        options = list(
            pageLength = 15)
        
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
