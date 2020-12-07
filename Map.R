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