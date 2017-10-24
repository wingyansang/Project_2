
shinyServer(function(input, output) {
   #reactive for map on Tab 1
    dt <- reactive({
    min_yr= input$range[1]
    max_yr= input$range[2]
    bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr, .N,by = .(primary_city, Zip, 
                                                                latitude, longitude)][order(N)] 
  })
    
    #reactive for info box on Tab 1
    dt2 <- reactive({
      min_yr= input$range[1]
      max_yr= input$range[2]
      bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr, .N,by = .(primary_city)][order(-N)] 
    })
    
    # show statistics using infoBox in Tab 1
    output$maxBox <- renderInfoBox({
      max_value <- max(dt2()$N)
      max_city <- dt2()$primary_city[1]
      infoBox(title = "First", max_city, max_value)
    })
    output$secBox <- renderInfoBox({
      sec_value <- dt2()$N[2]
      sec_city <- dt2()$primary_city[2]
      infoBox(title = "Second", sec_city, sec_value)
    })
    output$thirdBox <- renderInfoBox({
      third_value <- dt2()$N[3]
      third_city <- dt2()$primary_city[3]
      infoBox(title = "Third", third_city, third_value)
    })
    
    # show map using Leaflet in Tab 1
    output$map <-renderLeaflet({

    leaflet() %>% addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      clearShapes() %>%
      addCircleMarkers(data = dt(), ~longitude, ~latitude, 
                       radius=2, stroke=FALSE, fillOpacity= 1, fillColor = "Red", label = 
                         paste0("City: ", dt()$primary_city, "; Zip: ",dt()$Zip,"; Number Stolen: ",
                               dt()$N))  %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)})
    
    #reactive for map on Tab 2
    dt3 <- reactive({
      min_yr= input$range[1]
      max_yr= input$range[2]
      bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr, .N,by = .(State)][order(State)] 
    })
    
    # show map using GoogleVis in Tab 2
    output$map2 <- renderGvis({
      data <-  dt3()
      gvisGeoChart(data, "State", "N",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width="auto", height="auto"))
    })
    

    # total stolen bikes reactive used to calculate percentages in tab 3 graphs
      sumvar <- reactive({
        min_yr= input$range[1]
        max_yr= input$range[2]
        switch(input$variable, 
               Manufacturer = bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr & is.na(Manufacturer) == FALSE, .N], 
               Color = bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr & is.na(Color) == FALSE, .N], 
               Season = bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr & is.na(Season) == FALSE, .N],
               Lock_Type = bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr & is.na(Lock_desc) == FALSE, .N], 
               How_Stolen = bikes2_dt[year(Date)>=min_yr & year(Date)<=max_yr & is.na(Lock_circ) == FALSE, .N])
                          })

      
    # Currently selected variable for tab 3
      curvar <- reactive({
        min_yr= input$range[1]
        max_yr= input$range[2]
        switch(input$variable, 
               Manufacturer = bikes2_dt[year(Date) >= min_yr & year(Date) <= max_yr & 
                              is.na(Manufacturer) == FALSE, .N, by = .(Manufacturer)][order(-N)][1:10,], 
                Color = bikes2_dt[year(Date) >= min_yr & year(Date) <= max_yr & is.na(Color) == FALSE,.N, 
                        by = .(Color)][order(-N)], 
                Season = bikes2_dt[year(Date) >= min_yr & year(Date) <= max_yr & 
                          is.na(Season) == FALSE, .N, by = .(Season)][order(-N)],
                Lock_Type = bikes2_dt[year(Date) >= min_yr & year(Date) <= max_yr & 
                            is.na(Lock_desc) == FALSE, .N, by =.(Lock_desc)][order(-N)], 
                How_Stolen = bikes2_dt[year(Date) >= min_yr & year(Date) <= max_yr & 
                                is.na(Lock_circ) == FALSE, .N, by = .(Lock_circ)][order(-N)])
                      })
    
      # Render a barplot for tab 4
      output$varPlot <- renderPlot({
        barplot(curvar()$N*100/sumvar(), names.arg = curvar()[[1]],
            main = ifelse(input$range[1] != input$range[2],
                      paste0("Bar Chart Comparison: ",input$range[1]," to ", input$range[2]),
                      paste0("Bar Chart Comparison: ",input$range[1])), 
                    ylab="Percentage", xlab=input$variable, ylim = c(0, max(curvar()$N*100/sumvar())+10),
                col=topo.colors(length(curvar()[[1]])), cex.names = ifelse(input$variable == "How_Stolen", 1.0,1.25),
                cex.lab = 1.5, font.lab = 2, font.main = 2, cex.main = 1.75)
                                  })
      
      # Render word cloud for tab 4
        # Define a reactive expression for the document term matrix
        terms <- reactive({
          # Change when the "update" button is pressed...
          input$update
          # ...but not for anything else
          isolate({
            withProgress({
              setProgress(message = "Processing corpus...")
              getTermMatrix(input$selection)
            })
          })
        })
        
        # Make the wordcloud drawing predictable during a session
        wordcloud_rep <- repeatable(wordcloud)
        
        output$plot <- renderPlot({
          v <- terms()
          wordcloud_rep(names(v), v, scale=c(10,1),
                        min.freq = input$freq, max.words=input$max,
                        colors=brewer.pal(8, "Dark2"))
        })
      
      # Render a barplot for tab 5
      output$demo1Plot <- renderPlot({
        g <- ggplot(census_perc, aes(county, male, fill = factor(top25))) + 
          geom_bar(stat="identity", position = "dodge")
        
        g + coord_flip() + scale_fill_discrete(name = "Zip Code Ranked by Bikes Stolen", breaks=c("1", "0"), 
              labels = c("Top Quartile", "Bottom 3 Quartile")) + 
              scale_y_discrete(limits = c(0, 25, 50)) + 
              ggtitle("County Comparsion by Sex") + xlab("County") + ylab("Percent Male") + 
              theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              text = element_text(size=20), axis.title = element_text(face = "bold")) +
              theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
      })
      
      # Currently selected county for tab 6
      curCounty <- reactive({
        if (input$county == "D.C."){
          census_perc[county == "District of Columbia", .(top25, to24, to34, to44, to54, to64, over65)]
        } else if(input$county == "Orleans"){
          census_perc[county == "Orleans Parish", .(top25, to24, to34, to44, to54, to64, over65)]
        } else{
          census_perc[county == paste(input$county, "County", sep =" "), 
                                      .(top25, to24, to34, to44, to54, to64, over65)]}
        })
      
      # Render a barplot for tab 6
      output$demo2Plot <- renderPlot({
        
        data1 <- curCounty()
        data2 <- melt(data1, id.vars = "top25")
        
        g <- ggplot(data2, aes(variable, value, fill = factor(top25))) + 
          geom_bar(stat="identity", position = "dodge")
        
        g + scale_fill_discrete(name = "Zip Code Ranked by Bikes Stolen", breaks=c("1", "0"), 
                labels = c("Top Quartile", "Bottom 3 Quartile")) +  ylim(0, max(data2$value+5)) +
          scale_x_discrete(labels = c("0-24", "25-34", "35-44", "45-54", "55-64", "over 65"))+
          ggtitle("Age Distribution Comparison") + xlab("Age Group") + ylab("Percent") + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
                text = element_text(size=20), axis.title = element_text(face = "bold")) +
          theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
          geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
      })
      
      # Currently selected county for tab 7
      curCounty2 <- reactive({
        if (input$county2 == "D.C."){
          census_perc[county == "District of Columbia", .(top25, white, black, asian_PI, native_am, other)]
        } else if(input$county2 == "Orleans"){
          census_perc[county == "Orleans Parish", .(top25, white, black, asian_PI, native_am, other)]
        } else{
          census_perc[county == paste(input$county2, "County", sep =" "), 
                      .(top25, white, black, asian_PI, native_am, other)]}
      })
      
      #Render a barplot for tab 7
      output$demo3Plot <- renderPlot({
      
        data1 <- curCounty2()
        data2 <- melt(data1, id.vars = "top25")
      
      g <- ggplot(data2, aes(variable, value, fill = factor(top25))) + 
        geom_bar(stat="identity", position = "dodge")
      
      g + scale_fill_discrete(name = "Zip Code Ranked by Bikes Stolen", breaks=c("1", "0"), 
                              labels = c("Top Quartile", "Bottom 3 Quartile")) + 
        ylim(0,max(data2$value)+5) +
        scale_x_discrete(labels = c("White", "Black", "Asian_PI", "Native_American", "Other"))+
        ggtitle("Race Distribution Comparison") + xlab("Race") + ylab("Percent") + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
              text = element_text(size=20), axis.title = element_text(face = "bold")) +
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
      
      })
})
