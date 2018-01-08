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

shinyUI(dashboardPage(
  dashboardHeader(title = "H&R Block based on gender and age", titleWidth = 450),

  dashboardSidebar(
      selectInput(inputId = "pub_val", label = "Publisher Group", choices = c('Pub_Female_25_to_34','Pub_Male_25_to_34','Pub_Male_35_to_44','Pub_Female_35_to_44','Pub_Male_45_n_above','Pub_Female_45_n_above'), selected = 'Pub_Female_25_to_34'),
      selectInput(inputId = "app_val", label = "App Group", choices = c('App_Female_25_to_34','App_Male_25_to_34','App_Male_35_to_44','App_Female_35_to_44','App_Male_45_n_above','App_Female_45_n_above'), selected = 'App_Female_25_to_34'),

      sidebarMenu(
        menuItem("Google Play Store", href = "https://play.google.com/store?hl=en"),
        menuItem("Wikipedia", href = "https://en.wikipedia.org/wiki/Google_Play"))
  ),
  dashboardBody(
  		tabsetPanel(
  			tabPanel("Behavioral Index",
         		fluidRow(
         			chartOutput("behavCharts", "highcharts"))),

  			tabPanel("Metro Index",		
    			fluidRow(
    				chartOutput("Metro", "polycharts"))),

  			tabPanel("Categorical Index",
     			fluidRow(
					chartOutput("cateCharts", "highcharts"))),

  			tabPanel("HHI Level",
     			fluidRow(
     				chartOutput("HHI_Level", "nvd3"))),

  			tabPanel("Mosaic Index",
     			fluidRow(
     				chartOutput("mosaicCharts", "highcharts"))),


        tabPanel("Publishers",
          fluidRow(
            chartOutput("Pub_chart", "polycharts"))),
        
        tabPanel("Apps",
          fluidRow(
            chartOutput("Apps_chart", "polycharts")))
      		)
  		)
  	)
  )
