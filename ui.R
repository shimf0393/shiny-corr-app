library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(leaflet.extras)
library(magrittr)
library(shiny)
library(shinyjs)
library(miniUI)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(readr)
library(RColorBrewer)
library(leaflegend)
library(shinymanager)
library(googlesheets4)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
ui_map <-  
  fluidPage(
  tags$head(
    tags$style(
      HTML(".col-sm-5{padding-left:1px;}
           .col-sm-3{padding-left:1px;}
           .Toprow {padding-top: 12px; display:none;}
           
           .leaflet .legend i{
           border-radius: 50%;
           width: 10px;
           height: 10px;
           margin-top: 4px;}
           
           .info.legend.leaflet-control {direction:rtl;font-size:90%;}
           leaflet-top .info.legend.leaflet-control i div {margin-right:20%;}
          
           #downloadData.btn{font-size:10px; font-weight:bold;}
           .control-label {margin-bottom:20px;
           margin-top:5px;}
           
           #age_se {width: 67%; font-size: 80%;}
          .line_ipus .col-sm-1{padding-left:0%;}
          .col-sm-3 .box .box-body .row h2{padding-top:15px;}
          
          .switch_btn .form-group.shiny-input-container.shiny-input-container-inline{margin-bottom:3px;}
          .pretty.p-switch.p-slim .state:before{backgroud:background-image: initial !important;
                                                  background-position-x: initial !important;
                                                  background-position-y: initial !important;
                                                  background-size: initial !important;
                                                  background-repeat-x: initial !important;
                                                  background-repeat-y: initial !important;
                                                  background-attachment: initial !important;
                                                  background-origin: initial !important;
                                                  background-clip: initial !important;
                                                  background-color: rgb(184, 122, 204) !important;}
            .pretty.p-switch .state:before{border-top-color: rgb(184, 122, 204);
                                           border-top-style: solid;
                                           border-top-width: 1px;
                                           border-right-color: rgb(184, 122, 204);
                                           border-right-style: solid;
                                           border-right-width: 1px;
                                           border-bottom-color: rgb(184, 122, 204);
                                           border-bottom-style: solid;
                                           border-bottom-width: 1px;
                                           border-left-color: rgb(184, 122, 204);
                                           border-left-style: solid;
                                           border-left-width: 1px;
                                           border-image-source: initial;
                                           border-image-slice: initial;
                                           border-image-width: initial;
                                           border-image-outset: initial;
                                           border-image-repeat: initial;}
            .pretty.p-switch .state label:after {background-color: #b87acc!important;}
            
            .sliders {direction:rtl; font-size:80%;}
            
            .var1{display:none;}
            .var2{display:none;}
            .var1 .form-group.shiny-input-container .control-label{color:red; margin-bottom:10px;padding-right:33%;font-size:110%;}
            .col-sm-6 {height:104px;}
            .var2 .form-group.shiny-input-container .control-label{color:blue; margin-bottom:10px;padding-right:33%;;font-size:110%;}
            .p1 {padding-top:10%;}
            .p2 {padding-top:10%;}
            #reds_p img{width:90px;}
            #blues_p img{width:90px;}
            
            .leaflet-popup-content{direction:rtl;text-align:center;}
            .corr_label{display:none;padding-top:3%;font-weight:bold;font-size:110%;color:darkcyan/*darkslategray*/;}
            #corr_between_vars{width:50%;background-color:white;color:black;font-weight:bold;text-align:center;}
            }
             "
           
      )
    ),
    tags$script("setTimeout(function(){ $('.Toprow').show(); $('.var1').show();$('.var2').show();$('.corr_label').show();}, 8500);"
           )),
  
  fluidRow(class="Toprow", 
           div(box(
             checkboxGroupInput("migzar","בחר/י מגזר",choices =seq(1:6) ,selected =seq(1:6) ,inline = TRUE),
             width = 4),
             direction="rtl",align="center"),
           div(box(column(3,div(sliderInput("age_se","שנת בנייה",1930,2020,value = c(1930,2020),step = 5,sep =""),align="center")),
                   column(9,div(numericRangeInput("residents", "מספר תושבים בעיר", c(2000,1000000), width = 250, separator = " - "),align="center")),
                   width = 5),
               direction="rtl",align="right",style="font-size: 100%;",height="60%"
           ),
           box(
             div(downloadButton("downloadData","ייצוא נתונים",icon = icon("file-download")),align="right",style="font-size:120%; font-weight:bold;")#)
             , width = 3),
           box(fluidRow(titlePanel(div(actionLink("re_calc", "חישוב מחדש", icon= icon("calculator"),class = "my_class"),align="center", style="font-size:75%;font-weight:bold;")))
               ,width = 3),
           fluidRow(class="Midrow",
                    box(class="line_ipus",
                        width = 12,
                        column(9,div(class="switch_btn",
                                     tags$span("מתודולוגיית חישוב:",style="font-size:110%;font-weight:bold;margin-top:0px;margin-bottom:0px;margin-left:0%;direction:rtl;align:left;color:seagreen;"),
                                     prettySwitch(inputId = "calc_method",label = "חלוקת טווח",value = TRUE, inline = TRUE,status = "info",slim = FALSE),
                                     tags$span("עשירונים"),
                        ),style="font-size:80%;padding-right:2%;",align="right"),
                        column(3,actionLink("blank","")),
                    )
           ),
  ),
  column(width=9,withSpinner(leafletOutput("map", width = "100%", height = "750px"),type=3)),
  column(class="sliders",width = 3,
         column(class="p1",width = 6,imageOutput("reds_p")),
         column(class="var1",width = 6,selectInput("v1", "משתנה 1",
                                                   choice = c(seq(1:15)),
                                                   multiple = FALSE)),
         column(class="p2",width = 6,imageOutput("blues_p")),
         column(class="var2",width = 6,selectInput("v2", "משתנה 2",choice = c(seq(1:15)), multiple = FALSE)),
         column(class="corr",width = 6,verbatimTextOutput("corr_between_vars")),
         column(class="corr_label",width = 6,"מקדם המתאם בין המשתנים:")
         
         
         
  )
)



