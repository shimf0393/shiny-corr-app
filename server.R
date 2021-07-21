server <- function(input, output, session) {
  
  gs4_auth(path = 'credtials shinyapp-cf6f6eb8eb15.json')
  
  loadData <- function() {
    read_sheet("https://docs.google.com/spreadsheets/d/1NT8jM6TL_bxl_24Ay8kRBsS-scn98Dgsfj2g8L_jnts/edit#gid=0")
  }
  
  credentials<-loadData()
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  
  # Main login screen
  loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                   wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                       style = "text-align: center;",
                       actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                       shinyjs::hidden(
                         div(id = "nomatch",
                             tags$p("Incorrect username or password!",
                                    style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                    class = "text-center"))),
                       br(),
                       br(),
                       br()
                       
                     ))
  )
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$password)
          if(length(which(credentials$username==Username))==1) { 
            pasmatch  <- credentials["password"][which(credentials$username==Username),]
            pasverify <- pasmatch==Password
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      ####origin ui syntax of corr_app
      fluidPage(
        tags$head(
          tags$style(
            HTML(".col-sm-5{padding-left:1px;}
           .col-sm-3{padding-left:1px;}
           .Toprow {padding-top: 12px; /*display:none;*/}
           
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
            
            /*.var1{display:none;}
            .var2{display:none;}*/
            .var1 .form-group.shiny-input-container .control-label{color:red; margin-bottom:10px;padding-right:33%;font-size:110%;}
            .col-sm-6 {height:104px;}
            .var2 .form-group.shiny-input-container .control-label{color:blue; margin-bottom:10px;padding-right:33%;;font-size:110%;}
            .p1 {padding-top:10%;}
            .p2 {padding-top:10%;}
            #reds_p img{width:90px;}
            #blues_p img{width:90px;}
            
            .leaflet-popup-content{direction:rtl;text-align:center;}
            .corr_label{/*display:none;*/padding-top:3%;font-weight:bold;font-size:110%;color:darkcyan;}
            .col-sm-6.corr{padding-top:3%;}
            #corr_between_vars{width:50%;background-color:white;font-weight:bold;text-align:right;font-size:110%;}
            #corr_value{width:50%;font-weight:bold;text-align:left;font-size:110%;}
            }
             "
                 
            )
          )
        ),
        
        fluidRow(class="Toprow", 
                 div(box(
                   checkboxGroupInput("migzar","בחר/י מגזר",choices =seq(1:6) ,selected =seq(1:6) ,inline = TRUE),
                   width = 4),
                   direction="rtl",align="center"),
                 div(box(column(3,div(sliderInput("age_se","שנת בניה",1930,2020,value = c(1930,2020),step = 5,sep =""),align="center")),
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
               column(class="corr",width = 6,htmlOutput("corr_between_vars",inline = TRUE),htmlOutput("corr_value",inline = TRUE)),
               column(class="corr_label",width = 6,"מקדם המתאם בין המשתנים:")
               
               
               
        )
      )
      
      
    }
    else {
      loginpage
    }
  })
  
  choice_names <<- c("מדד פריפריאליות","מחיר מר","שנת בנייה", "רגישות ססמוגרפית", "דירוג חום- אי נוחות", "מבנים בפשט הצפה","דרוג סוציו-אקונומי ברמת אס","דרוג סוציו-אקונומי ברמת ישוב", "מקבלי הבטחת הכנסה", "מקבלי השלמת הכנסה", "מקבלי אבטלה", "מקבלי סיוע בדיור", "דירות בדיור ציבורי", "מחלות עוני","עלייה מאתיופיה")
  full_data<<-read.csv("data/complete_data.csv", header = TRUE, row.names = NULL, stringsAsFactors = FALSE, encoding = "UTF-8")
  #full_data<<-read.csv("c:/TEST/corr_app/data/complete_data.csv", header = TRUE, row.names = NULL, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  grade_for_map<-"total_grade_cat1"
  
  observeEvent(USER$login == TRUE,
               lapply(1:15,function(i)
               { updateCheckboxGroupInput(session,"migzar","בחר/י מגזר",selected =seq(1:6) ,inline = TRUE, choiceNames=migzar_names, choiceValues= seq(1:6) )
                 updateSliderInput(session,"age_se", "שנת בניה",min_year,max_year,value = c(min_year,max_year),step = 5)
                 updateSelectInput(session,"v1","משתנה 1",choices =choice_names)
               }))
  
  
  full_data$residents_yeshuv<-as.numeric(full_data$residents_yeshuv)
  full_data$residents_se<-as.numeric(full_data$residents_se)
  full_data$migzar<-1
  for (i in 1:nrow(full_data))
  {if(full_data$demogrphia[i]=="דרוזי") {full_data$migzar[i]<-2}
    if(full_data$demogrphia[i]=="יהודי- כללי") {full_data$migzar[i]<-3}
    if(full_data$demogrphia[i]=="בדואי") {full_data$migzar[i]<-4}
    if(full_data$demogrphia[i]=="חרדי") {full_data$migzar[i]<-5}
    if(full_data$demogrphia[i]=="צ'רקסי") {full_data$migzar[i]<-6}} 
  
  
  full_data$x<-as.numeric(full_data$x)
  full_data$y<-as.numeric(full_data$y)
  migzar_names<-unique(full_data$demogrphia)
  
  ############׳³ֲ¢׳³ג€׳³ג€÷׳³ג€¢׳³ֲ ׳³ג€׳³ֲ¨׳³ֲ©׳³ג„¢׳³ֲ׳³ג€¢׳³ֳ— ׳³ֲ׳³ֲ©׳³ֲ׳³ג€¢׳³ֳ—
  
  ############׳³ֲ¨׳³ג‚×׳³ֲ¨׳³ג€¢׳³ֲ© ׳³ֲ¨׳³ֲ©׳³ג„¢׳³ֲ׳³ג€ ׳³ֲ©׳³ֲ ׳³ֲ׳³ֲ©׳³ֳ—׳³ֲ ׳³ג€ 2 ׳³ג€˜׳³ג€׳³ֳ—׳³ֲ׳³ֲ ׳³ֲ׳³ֲ׳³ֲ©׳³ֳ—׳³ֲ ׳³ג€ 1
  observeEvent(input$v1,lapply(1:15,function(i)
  { if (input$v1==choice_names[i])
  {update_list<-choice_names[-i]
  updateSelectInput(session,"v2","משתנה 2",choices = update_list )}}
  ))
  
  
  ###reorder col
  reorder_col<-c(1:9,12,11,10,13,14,16:20,22,21,23,24,15,25,26)
  full_data<-full_data[,reorder_col]
  
  
  full_data$v1<-as.numeric(full_data$madad_periferya)
  full_data$v2<-as.numeric(full_data$avg_price_mr)
  full_data$v3<-as.numeric(as.integer(format(Sys.Date(),"%Y"))-full_data$year_bniya)*(-1)
  full_data$v4<-as.numeric(full_data$sesmo_sensitive)*(-1)
  full_data$v5<-as.numeric(full_data$derug_chom)*(-1)
  full_data$v6<-as.numeric(full_data$mivnim_peshat_hatzafa)
  full_data$v7<-as.numeric(full_data$derug_lamas_se)
  full_data$v8<-as.numeric(full_data$derug_lamas_yeshuv)
  full_data$v9<-as.numeric(full_data$havtachat_hachnasa)*(100)
  full_data$v10<-as.numeric(full_data$hashlamat_hachnasa)*(100)
  full_data$v11<-as.numeric(full_data$avtala)*(100)
  full_data$v12<-as.numeric(full_data$siyua_bediyur)*(100)
  full_data$v13<-as.numeric(full_data$diyur_tizburi)*(1)
  full_data$v14<-as.numeric(full_data$machalot_oni)*(-1)
  full_data$v15<-as.numeric(full_data$aliya)*(-1)
  n_row<-as.numeric(nrow(full_data))
  
  #####׳³ג€”׳³ֲ׳³ג€¢׳³ֲ§׳³ֳ— ׳³ג€÷׳³ֲ ׳³ֲ׳³ֲ©׳³ֳ—׳³ֲ ׳³ג€ ׳³ֲ׳³ֲ¢׳³ֲ©׳³ג„¢׳³ֲ¨׳³ג€¢׳³ֲ ׳³ג„¢׳³ֲ ׳³ֲ׳³ג‚×׳³ג„¢ ׳³ֻ׳³ג€¢׳³ג€¢׳³ג€”
  for (p in 1:15)
  { 
    full_data[[paste0("range_bin",p)]]<-0
    input_p<-full_data[[paste0("v",p)]]
    value_to_start<-min(input_p)
    assign("bin_size",(max(input_p)-min(input_p))/4)
    for (i in 1:4)
    {bin<-assign(paste0("bin_",i),value_to_start+bin_size)
    value_to_start<-value_to_start+bin_size
    for (j in 1:n_row)  
    {if (round(input_p[j],7)<=round(bin,7) & full_data[j,p+41]==0) {full_data[j,p+41]<-i}} 
    }
  }
  
  #####׳³ג€”׳³ֲ׳³ג€¢׳³ֲ§׳³ֳ— ׳³ג€÷׳³ֲ ׳³ֲ׳³ֲ©׳³ֳ—׳³ֲ ׳³ג€ ׳³ֲ׳³ֲ¢׳³ֲ©׳³ג„¢׳³ֲ¨׳³ג€¢׳³ֲ ׳³ג„¢׳³ֲ ׳³ֲ׳³ג‚×׳³ג„¢ ׳³ֲ׳³ג€”׳³ג€¢׳³ג€“׳³ג€¢׳³ֲ ׳³ג„¢׳³ֲ  
  for (p in 1:15)
  { 
    full_data[[paste0("q_bin",p)]]<-0
    input_p<-full_data[[paste0("v",p)]]
    q_bins<-quantile(full_data[,p+26], probs = seq(0, 1, 0.25), na.rm = TRUE)
    for (i in 1:4)
    {
      bin<-assign(paste0("bin_",i),q_bins[i+1])
      for (j in 1:n_row)  
      {if (round(input_p[j],7)<=round(bin,7) & full_data[j,p+56]==0) {full_data[j,p+56]<-i}} 
    }
  }
  
  ####### ׳³ֲ׳³ג€׳³ג€÷׳³ֲ ׳³ג„¢׳³ֲ¡ ׳³ג‚×׳³ג€ ׳³ֲ׳³ג€¢׳³ג€™׳³ג„¢׳³ֲ§׳³ג€ ׳³ֲ©׳³ֲ ׳³ג€”׳³ֲ׳³ג€¢׳³ֲ§׳³ֳ— ׳³ֲ¦׳³ג„¢׳³ג€¢׳³ֲ ׳³ג„¢׳³ֲ
  grade_range<-0
  for(i in seq(10,40,10))
  {for(j in 1:4)
    grade_range<-rbind(grade_range,i+j)}
  grade_range<-grade_range[-1]
  
  full_data$total_grade_cat1<-match(full_data$range_bin1*10+full_data$range_bin2,grade_range)
  full_data$total_grade_cat2<-match(full_data$q_bin1*10+full_data$q_bin2,grade_range)
  
  ###############stage 4- create_heatmap########################
  full_data$long<-as.numeric(full_data$x)
  full_data$lat<-as.numeric(full_data$y)
  min_year<-min(full_data$year_bniya)
  max_year<-max(full_data$year_bniya)
  min_res<-min(full_data$residents_yeshuv)
  max_res<-max(full_data$residents_yeshuv)
  full_data$residents_cat<<-0
  
  for (i in 1:nrow(full_data))
  {if(full_data$residents_se[i]<1000) {full_data$residents_cat[i]<-150}
    if(full_data$residents_se[i]>=1000 & full_data$residents_se[i]<10000) {full_data$residents_cat[i]<-300}
    if(full_data$residents_se[i]>=10000) {full_data$residents_cat[i]<-800}} 
  
  data_for_heatmap<<-full_data
  
  
  reds<-c(255,143,21,0,255,211,150,78,255,231,163,85,255,210,161,112)
  greens<-c(255,207,155,112,189,181,140,55,117,153,101,46,0,66,44,48)
  blues<-c(255,255,255,192,189,233,224,195,117,177,209,162,0,135,164,160)
  rgb_p<-rgb(reds,greens,blues, maxColorValue = 255)
  
  beatCol <- colorFactor(palette = rgb_p, c(1:16))
  
  calc_method<-reactive({ 
    if (input[["calc_method"]]==TRUE) {grade_for_map<<-"total_grade_cat1"}
    else {grade_for_map<<-"total_grade_cat2"}
    
    if (input[["calc_method"]]==TRUE) {calc_type<<-"range_bin"}
    else {calc_type<<-"q_bin"}
    
  })
  
  p1<-1
  p2<-2
  
  ######################################
  updatedData <- reactive({
    lapply(1:15,function(i)
    { if(input[["v1"]]==choice_names[i]) {p1<<-i}
      if(input[["v2"]]==choice_names[i]) {p2<<-i}})
    range_v1<-full_data[[paste0("range_bin",p1)]]
    range_v2<-full_data[[paste0("range_bin",p2)]]
    q_v1<-full_data[[paste0("q_bin",p1)]]
    q_v2<-full_data[[paste0("q_bin",p2)]]
    full_data$total_grade_cat1<<-match(range_v1*10+range_v2,grade_range)
    full_data$total_grade_cat2<<-match(q_v1*10+q_v2,grade_range)
    
    data_for_heatmap<<-full_data
    
    ####׳³ג€׳³ג‚×׳³ֲ¢׳³ֲ׳³ֳ— ׳³ג€׳³ֲ¡׳³ג„¢׳³ֲ ׳³ג€¢׳³ֲ ׳³ג„¢׳³ֲ  
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$residents_yeshuv>=input$residents[1] & data_for_heatmap$residents_yeshuv<=input$residents[2],]
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$migzar%in%input$migzar,]
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$year_bniya>=input$age_se[1] & data_for_heatmap$year_bniya<=input$age_se[2],]
    
  })
  
  # corr_vars<-reactive({
  #   lapply(1:15,function(i)
  #   { if(input[["v1"]]==choice_names[i]) {p1<<-i}
  #     if(input[["v2"]]==choice_names[i]) {p2<<-i}})
  #   round(cor(full_data[,26+p1],full_data[,26+p2],use = "all.obs"),3)})
  # 
  # 
  # output$corr_between_vars<-renderText(corr_vars())
  
  corr_vars<-reactive({
    lapply(1:15,function(i)
    { if(input[["v1"]]==choice_names[i]) {p1<<-i}
      if(input[["v2"]]==choice_names[i]) {p2<<-i}})
    x<-round(cor(full_data[,8+p1],full_data[,8+p2],use = "all.obs"),2)
  })
  
  corr_b_vars<-reactive({
    x<-corr_vars()
    if (abs(x)<=0.1) {paste("<font color=\"#808080\"><b>","אפסי","</b></font>")}
    else if (abs(x)<=0.3) {paste("<font color=\"#1CBBDD\"><b>","חלש","</b></font>")}
    else if (abs(x)<=0.5 & x<0) {paste("<font color=\"#FFA500\"><b>","בינוני","</b></font>")}
    else {paste("<font color=\"#FF0000\"><b>","חזק","</b></font>")}
  })
  
  p_corr_vars<-reactive({
    x<-corr_vars()
    if (abs(x)<=0.1 & x<0) {paste("<font color=\"#808080\"><b>","(",abs(x),"-)","</b></font>")}
    else if (abs(x)<=0.1 & x>=0) {paste("<font color=\"#808080\"><b>","(",x,")","</b></font>")}
    else if (abs(x)<=0.3 & x<0) {paste("<font color=\"#1CBBDD\"><b>","(",abs(x),"-)","</b></font>")}
    else if (abs(x)<=0.3 & x>0) {paste("<font color=\"#1CBBDD\"><b>","(",x,")","</b></font>")}
    else if (abs(x)<=0.5 & x<0) {paste("<font color=\"#FFA500\"><b>","(",abs(x),"-)","</b></font>")}
    else if (abs(x)<=0.5 & x>0) {paste("<font color=\"#FFA500\"><b>","(",x,")","</b></font>")}
    else if (abs(x)>0.5 & x<0) {paste("<font color=\"#FF0000\"><b>","(",abs(x),"-)","</b></font>")}
    else {paste("<font color=\"#FF0000\"><b>"," (",x,")","</b></font>")}
  })
  
  output$corr_between_vars<-renderText(corr_b_vars())
  output$corr_value<-renderText(p_corr_vars())
  observeEvent(
    eventExpr = input$map_zoom, {
      leafletProxy(
        mapId = "map",
        session = session
      )%>% clearShapes() %>%
        addCircles(data=data_for_heatmap,lng = data_for_heatmap$long, lat = data_for_heatmap$lat,
                   weight = case_when(input$map_zoom <=4 ~1,
                                      input$map_zoom ==5 ~2,
                                      input$map_zoom ==6 ~3,
                                      input$map_zoom ==7 ~5,
                                      input$map_zoom ==8 ~7,
                                      input$map_zoom ==9 ~9,
                                      input$map_zoom ==10 ~7,
                                      input$map_zoom ==11 ~3,
                                      input$map_zoom >11 ~1),
                   popup =  paste(data_for_heatmap$yeshuv,"<br>",data_for_heatmap$schuna,'-(',"<font color='red'>",data_for_heatmap[,paste0(calc_type,p1)],"<font color='black'>",",","<font color='blue'>",data_for_heatmap[,paste0(calc_type,p2)],"<font color='black'>",")"),
                   color = beatCol(data_for_heatmap[,grade_for_map]), radius = data_for_heatmap$residents_cat,
                   fillOpacity = 0.5)
      
      session$onSessionEnded(function() {stopApp()})
    }
  )
  
  
  
  ####׳³ֲ³ײµֲ¾׳³ֲ³ײ²ֲ§׳³ֲ³ײ²ֲ¨׳³ֲ³ײ²ֲ ׳³ֲ³ײµג€׳³ֲ³׳’ג‚¬ג„¢׳³ֲ³׳’ג‚¬ֲ¢׳³ֲ³׳’ג‚¬ֲ׳³ֲ³ײµג€ ׳³ֲ³ײ²ֲ¨׳³ֲ³׳’ג‚¬ֲ׳³ֲ³׳’ג€ֲ¢׳³ֲ³׳’ג‚¬ֲ¢׳³ֲ³ײ²ֲ¡
  addLegendCustom <- function(map,colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, title="מספר תושבים באזור סטטיסטי",opacity = opacity))}
  
  legend_path<-"www/combined_palette_map.jpg"
  #legend_path<-"c:/TEST/corr_app/www/combined_palette_map.jpg"
  
  output$map <- renderLeaflet({
    leaflet()%>%
      addLegendImage(image=legend_path, labels = c(''),width = 150, height = 150,
                     title = htmltools::tags$div('הקשר בין המשתנים',
                                                 style = 'font-size: 14px; text-align: center;'),
                     position = 'bottomright')%>%
      addLegendCustom(colors = c("navy", "navy", "navy"), labels = c("עד 1,000", "1,000-10,000", "10,000 ומעלה"), sizes = c(8, 15, 30))%>%
      addTiles()%>%
      setView(lng = 35.1077, lat = 31.80651, zoom = 7)
  })
  
  
  observeEvent(input$calc_method,
               {calc_method()
                 leafletProxy("map", data = data_for_heatmap) %>%
                   clearShapes() %>%
                   addCircles(lng = ~long, lat = ~lat,
                              weight = case_when(input$map_zoom <=4 ~1,
                                                 input$map_zoom ==5 ~2,
                                                 input$map_zoom ==6 ~3,
                                                 input$map_zoom ==7 ~5,
                                                 input$map_zoom ==8 ~7,
                                                 input$map_zoom ==9 ~9,
                                                 input$map_zoom <12 ~9,
                                                 input$map_zoom >11 ~1),
                              popup = paste(data_for_heatmap$yeshuv,"<br>",data_for_heatmap$schuna,"<br>",'-(',"<font color='red'>",data_for_heatmap[,paste0(calc_type,p1)],"<font color='black'>",",","<font color='blue'>",data_for_heatmap[,paste0(calc_type,p2)],"<font color='black'>",")"),
                              color = beatCol(data_for_heatmap[,grade_for_map]), radius = data_for_heatmap[,"residents_cat"],
                              fillOpacity = 0.5)
               }
  )
  
  
  observeEvent(input$re_calc,
               { calc_method()
                 u_data<-updatedData()
                 leafletProxy("map", data = updatedData()) %>%
                   clearShapes() %>%
                   addCircles(lng = ~long, lat = ~lat,
                              weight = case_when(input$map_zoom <=4 ~1,
                                                 input$map_zoom ==5 ~2,
                                                 input$map_zoom ==6 ~3,
                                                 input$map_zoom ==7 ~5,
                                                 input$map_zoom ==8 ~7,
                                                 input$map_zoom ==9 ~9,
                                                 input$map_zoom <12 ~9,
                                                 input$map_zoom >11 ~1),
                              popup = paste(data_for_heatmap$yeshuv,"<br>",data_for_heatmap$schuna,"<br>",'-(',"<font color='red'>",data_for_heatmap[,paste0(calc_type,p1)],"<font color='black'>",",","<font color='blue'>",data_for_heatmap[,paste0(calc_type,p2)],"<font color='black'>",")"),
                              color = beatCol(u_data[,grade_for_map]), radius = u_data[,"residents_cat"],
                              fillOpacity = 0.5)
               })
  
  
  
  ###׳³ֲ³ײ²ֲ§׳³ֲ³׳’ג‚¬ֲ¢׳³ֲ³׳’ג‚¬ֻ׳³ֲ³ײ²ֲ¥ ׳³ֲ³׳’ג‚¬ֲ׳³ֲ³׳’ג€ֲ¢׳³ֲ³ ׳³ֲ³ײµֲ¾׳³ֲ³׳’ג€ֲ¢ ׳³ֲ³ײµג€׳³ֲ³ײ²ֲ¦׳³ֲ³׳’ג‚¬ֲ¢׳³ֲ³ײ²ֲ¨׳³ֲ³ײµֲ¡ ׳³ֲ³׳’ג‚¬ֲ׳³ֲ³׳’ג‚¬ֲ׳³ֲ³׳’ג‚¬ֲ¢׳³ֲ³ײ²ֲ¨׳³ֲ³׳’ג‚¬ֲ׳³ֲ³׳’ג‚¬ֲ
  data_for_download<- reactive({
    calc_method()
    updatedData()
    if (grade_for_map=="total_grade_cat1")
    {data_for_file<<-data_for_heatmap[order(data_for_heatmap$total_grade_cat1),]}
    else
    {data_for_file<<-data_for_heatmap[order(data_for_heatmap$total_grade_cat2),]}
    base_col<-c(1,3,6,8,25)
    ###׳³ֲ¢׳³ֲ׳³ג€¢׳³ג€׳³ג€¢׳³ֳ— ׳³ג€׳³ֲ׳³ֲ©׳³ֳ—׳³ֲ ׳³ג„¢׳³ֲ ׳³ֲ©׳³ֲ ׳³ג€˜׳³ג€”׳³ֲ¨׳³ג€¢
    p_col<-c(9:23)
    c_p_col<-c(p_col[p1],p_col[p2])
    ####׳³ֲ¢׳³ֲ׳³ג€¢׳³ג€׳³ג€¢׳³ֳ— ׳³ג„¢׳³ג„¢׳³ֲ¦׳³ג€¢׳³ֲ ׳³ֲ¢׳³ג‚×׳³ג„¢ ׳³ֲ©׳³ג„¢׳³ֻ׳³ֳ— ׳³ג€׳³ג€”׳³ג„¢׳³ֲ©׳³ג€¢׳³ג€˜
    if (grade_for_map=="total_grade_cat1") {col_bin<-c(41+p1,41+p2)}
    else {col_bin<-c(56+p1,56+p2)}
    #####׳³ג€÷׳³ֲ׳³ֲ ׳³ג€׳³ֲ¢׳³ֲ׳³ג€¢׳³ג€׳³ג€¢׳³ֳ—
    col_for_download<-c(base_col,c_p_col,col_bin)
    
    data_for_file<<-data_for_file[,col_for_download]
    data_for_file
  })
  
  output$downloadData <- downloadHandler(
    filename<-  function() {
      gsub(":","_",gsub("-","_",paste0("params_conections",Sys.time(),".csv")))
      
    },
    content = function(file) {
      write_excel_csv(data_for_download(),file)
    }
    
  )
  
  ####׳³ג‚×׳³ֲ׳³ֻ׳³ג€ ׳³ֲ©׳³ֲ 4 ׳³ֲ¦׳³ג€˜׳³ֲ¢׳³ג„¢׳³ֲ
  REDS_path<-"www/REDS4P2.jpg"
  BLUES_path<-"www/BLUES4P2.jpg"
  
  output$reds_p<-renderImage({
    list(src = REDS_path,contentType="image/jpg",width="120px",height="30px")
  }, deleteFile = FALSE)
  
  output$blues_p<-renderImage({
    list(src = BLUES_path,contentType="image/jpg",width="120px",height="30px")
  }, deleteFile = FALSE)
  
  session$onSessionEnded(function() {stopApp()})
  
  
  
}

