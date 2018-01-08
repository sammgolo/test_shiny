library(shiny)
library(XLConnect)
library(devtools)
library(rCharts)
library(Rcpp)
library(magrittr)
library(dplyr)
library(viridisLite)
library(rCharts)
library(tidyr)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(rsconnect)

shinyServer(function(input, output, session) {
	wb <- loadWorkbook("H&R Block Intelligence Report.xlsx")
	lst = readWorksheet(wb, sheet = getSheets(wb))

	Pub_Female_25_to_34 = lst$'Pub_Female_25_to_34'[order(-(lst$'Pub_Female_25_to_34')$Index), ][1:5,]
	Pub_Male_25_to_34 = lst$'Pub_Male_25_to_34'[order(-(lst$'Pub_Male_25_to_34')$Index), ][1:5,]
	Pub_Male_35_to_44 = lst$'Pub_Male_35_to_44'[order(-(lst$'Pub_Male_35_to_44')$Index), ][1:5,]
	Pub_Female_35_to_44 = lst$'Pub_Female_35_to_44'[order(-(lst$'Pub_Female_35_to_44')$Index), ][1:5,]
	Pub_Male_45_n_above = lst$'Pub_Male_45&above'[order(-(lst$'Pub_Male_45&above')$Index), ][1:5,]
	Pub_Female_45_n_above = lst$'Pub_Female_45&above'[order(-(lst$'Pub_Female_45&above')$Index), ][1:5,]
	App_Female_25_to_34 = lst$'App_Female_25_to_34'[order(-(lst$'App_Female_25_to_34')$Index), ][1:5,]
	App_Male_25_to_34 = lst$'App_Male_25_to_34'[order(-(lst$'App_Male_25_to_34')$Index), ][1:5,]
	App_Female_35_to_44 = lst$'App_Female_35_to_44'[order(-(lst$'App_Female_35_to_44')$Index), ][1:5,]
	App_Male_35_to_44 = lst$'App_Male_35_to_44'[order(-(lst$'App_Male_35_to_44')$Index), ][1:5,]
	App_Female_45_n_above = lst$'App_Female_45&above'[order(-(lst$'App_Female_45&above')$Index), ][1:5,]
	App_Male_45_n_above = lst$'App_Male_45&above'[order(-(lst$'App_Male_45&above')$Index), ][1:5,]

	Behavioral_Index = lst$'Behavioral_Index'
	Category_Index = lst$'Category_Index'
	House_Hold_Income = lst$'House Hold Income'
	Mosaics = lst$'Mosaics'
	Metro_Index = lst$'Metro_Index'[complete.cases(lst$'Metro_Index'),]
	Metro_Index_explode = gather(Metro_Index, key, number, -MSA_Name)
	Category_Index_explode = gather(Category_Index, key, number, -Category)
	House_Hold_Income_explode = gather(House_Hold_Income, key, number, -HHI_Level)


	Pub_Female_25_to_34$key = 'Pub_Female_25_to_34'
	Pub_Male_25_to_34$key = 'Pub_Male_25_to_34'
	Pub_Male_35_to_44$key = 'Pub_Male_35_to_44'
	Pub_Female_35_to_44$key = 'Pub_Female_35_to_44'
	Pub_Male_45_n_above$key = 'Pub_Male_45_n_above'
	Pub_Female_45_n_above$key = 'Pub_Female_45_n_above'

	App_Female_25_to_34$key = 'App_Female_25_to_34'
	App_Male_25_to_34$key = 'App_Male_25_to_34'
	App_Male_35_to_44$key = 'App_Male_35_to_44'
	App_Female_35_to_44$key = 'App_Female_35_to_44'
	App_Male_45_n_above$key = 'App_Male_45_n_above'
	App_Female_45_n_above$key = 'App_Female_45_n_above'

	publishers = rbind(Pub_Female_25_to_34,Pub_Male_25_to_34,Pub_Male_35_to_44,Pub_Female_35_to_44,Pub_Male_45_n_above,Pub_Female_45_n_above)
	apps= rbind(App_Female_25_to_34,App_Male_25_to_34,App_Male_35_to_44,App_Female_35_to_44,App_Male_45_n_above,App_Female_45_n_above)


	hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab') 

 	output$behavCharts = renderChart2({
		hc_behavioral = Highcharts$new() 
		hc_behavioral$title(text=paste("Behavioral Index"), style=list(color="#000000"))
  		hc_behavioral$chart(type = "spline")
  	
  		hc_behavioral$series(name = "Female_24_to_34", data = Behavioral_Index$'Female.25...34', dataLabels = list(enabled = TRUE))
  		hc_behavioral$series(name = "Female_35_to_44", data = Behavioral_Index$'Female.35...44', dataLabels = list(enabled = TRUE))
  		hc_behavioral$series(name = "Female_45_to_54", data = Behavioral_Index$'Female.45...54', dataLabels = list(enabled = TRUE))
  		hc_behavioral$series(name = "Male_25_to_34", data = Behavioral_Index$'Male.25...34', dataLabels = list(enabled = TRUE))
  		hc_behavioral$series(name = "Male_35_to_44", data = Behavioral_Index$'Male.35...44', dataLabels = list(enabled = TRUE))
  		hc_behavioral$series(name = "Male_45_to_54", data = Behavioral_Index$'Male.45...54', dataLabels = list(enabled = TRUE))
  		hc_behavioral$legend(verticalAlign="top", y=50, itemStyle=list(color="gray"))
  		hc_behavioral$xAxis(categories = Behavioral_Index$SegmentName)
  		hc_behavioral$params$width <- 1000
    	hc_behavioral$params$height <- 500
		return(hc_behavioral)
	})

  	output$Metro = renderChart({
    	p6 = rPlot(MSA_Name ~ key, color = 'number', data = Metro_Index_explode, type = 'tile', height = 800)

		p6$guides(y = list(title = "", ticks = unique(Metro_Index_explode$MSA_Name)))
    	p6$addParams(height = 300, dom = 'Metro', title = "Metro Index")
    	p6$params$width <- 1000
    	p6$params$height <- 500
    	return(p6)
	}) 

 	output$cateCharts = renderChart2({
		hc_categorical = Highcharts$new() 
		hc_categorical$title(text=paste("Categorical Index"), style=list(color="#000000"))
  		hc_categorical$chart(type = "spline")
  	
  		hc_categorical$series(name = "Female_24_to_34", data = Category_Index$'Female.25...34', dataLabels = list(enabled = TRUE))
  		hc_categorical$series(name = "Female_35_to_44", data = Category_Index$'Female.35...44', dataLabels = list(enabled = TRUE))
  		hc_categorical$series(name = "Female_45_to_54", data = Category_Index$'Female.45...54', dataLabels = list(enabled = TRUE))
  		hc_categorical$series(name = "Male_25_to_34", data = Category_Index$'Male.25...34', dataLabels = list(enabled = TRUE))
  		hc_categorical$series(name = "Male_35_to_44", data = Category_Index$'Male.35...44', dataLabels = list(enabled = TRUE))
  		hc_categorical$series(name = "Male_45_to_54", data = Category_Index$'Male.45...54', dataLabels = list(enabled = TRUE))
  		hc_categorical$legend(verticalAlign="top", y=50, itemStyle=list(color="gray"))
  		hc_categorical$xAxis(categories = Category_Index$Category)
  		hc_categorical$params$width <- 1000
    	hc_categorical$params$height <- 500
		return(hc_categorical)
	})

	output$HHI_Level = renderChart2({
		p2 = nPlot(number ~ HHI_Level, group = 'key', data = House_Hold_Income_explode, type = 'multiBarChart')
		p2$chart(color = c('brown', 'blue','green','red', 'purple', '#594c26'), reduceXTicks = FALSE)
		p2$xAxis(tickValues = "#! function (x) {
    		tickValues = ['< 25k','25k - 49k', '50k - 74k', '75k - 99k', '100k - 124k', '124k - 150k', '> 150k'];
    		return tickValues;
			} !#")
		p2$set(width = 600)
		return(p2)
	})

	output$mosaicCharts = renderChart2({
		hc_mosaic = Highcharts$new() 
		hc_mosaic$title(text=paste("Mosaics"), style=list(color="#000000"))
  		hc_mosaic$chart(type = "spline")
  	
  		hc_mosaic$series(name = "Female_24_to_34", data = Mosaics$'Female.25...34', dataLabels = list(enabled = TRUE))
  		hc_mosaic$series(name = "Female_35_to_44", data = Mosaics$'Female.35...44', dataLabels = list(enabled = TRUE))
  		hc_mosaic$series(name = "Female_45_to_54", data = Mosaics$'Female.45...54', dataLabels = list(enabled = TRUE))
  		hc_mosaic$series(name = "Male_25_to_34", data = Mosaics$'Male.25...34', dataLabels = list(enabled = TRUE))
  		hc_mosaic$series(name = "Male_35_to_44", data = Mosaics$'Male.35...44', dataLabels = list(enabled = TRUE))
  		hc_mosaic$series(name = "Male_45_to_54", data = Mosaics$'Male.45...54', dataLabels = list(enabled = TRUE))
  		hc_mosaic$legend(verticalAlign="top", y=50, itemStyle=list(color="gray"))
  		hc_mosaic$xAxis(categories = Mosaics$'Mosaic.Name')
  		hc_mosaic$params$width <- 1000
    	hc_mosaic$params$height <- 500
		return(hc_mosaic)
	})

  output$Pub_chart = renderChart2({
    pub = rPlot(Index ~ Publisher, color = "Publisher", data = subset(publishers, key == as.character(input$pub_val)), type = 'bar')
    return(pub)
  })

    output$Apps_chart = renderChart2({
    app = rPlot(Index ~ appName, color = "appName", data = subset(apps, key == as.character(input$app_val)), type = 'bar')
    return(app)
  })

  }
)
  
