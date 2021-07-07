
server_map <- function(input, output,session) {
   #gs4_auth(path = 'credtials shinyapp-cf6f6eb8eb15.json')

  #loadData <- function() {
    # Read the data
  # read_sheet("https://docs.google.com/spreadsheets/d/1NT8jM6TL_bxl_24Ay8kRBsS-scn98Dgsfj2g8L_jnts/edit#gid=0")
  #}
  # call the server part
  #check_credentials returns a function to authenticate users
   # res_auth <- secure_server(
   #   check_credentials = check_credentials(loadData())
   # )
   # 
   # output$auth_output <- renderPrint({
   #   reactiveValuesToList(res_auth)
   # }) 
  
  
  choice_names <<- c("מדד פריפריאליות","מחיר מר","שנת בנייה","רגישות ססמוגרפית","דירוג חום- אי נוחות","מבנים בפשט הצפה", "דרוג סוציו-אקונומי ברמת אס", "דרוג סוציו-אקונומי ברמת ישוב","מקבלי הבטחת הכנסה", "מקבלי השלמת הכנסה","מקבלי אבטלה","מקבלי סיוע בדיור","דירות בדיור ציבורי","מחלות עוני","עלייה מאתיופיה")
  full_data<<-read.csv("data/complete_data.csv", header = TRUE, row.names = NULL, stringsAsFactors = FALSE, encoding = "UTF-8")
  grade_for_map<-"total_grade_cat1"
  
  #####׳³ֲ©׳³ֵ¾׳³ג€¢׳³ֳ— ׳³ג€¢׳³ֳ—׳³ֲ¦׳³ג€¢׳³ג€™׳³ג€ ׳³ג€˜׳³ג€׳³ֲ¢׳³ֵ“׳³ֲ׳³ג€ ׳³ֲ¨׳³ֲ׳³ֲ©׳³ג€¢׳³ ׳³ג„¢׳³ֳ— ׳³ֲ©׳³ֵ“ ׳³ג€׳³ֲ׳³ג‚×׳³ֵ“׳³ג„¢׳³ֲ§׳³ֲ¦׳³ג„¢׳³ג€
  observeEvent('shiny:sessioninitialized',lapply(1:15,function(i)
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
  
  ############׳¢׳“׳›׳•׳ ׳”׳¨׳©׳™׳׳•׳× ׳׳©׳׳•׳×
  
  ############׳¨׳₪׳¨׳•׳© ׳¨׳©׳™׳׳” ׳©׳ ׳׳©׳×׳ ׳” 2 ׳‘׳”׳×׳׳ ׳׳׳©׳×׳ ׳” 1
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
  full_data$v6<-as.numeric(full_data$mivnim_peshat_hatzafa)*(-100)
  full_data$v7<-as.numeric(full_data$derug_lamas_se)
  full_data$v8<-as.numeric(full_data$derug_lamas_yeshuv)
  full_data$v9<-as.numeric(full_data$havtachat_hachnasa)*(-100)
  full_data$v10<-as.numeric(full_data$hashlamat_hachnasa)*(-100)
  full_data$v11<-as.numeric(full_data$avtala)*(-100)
  full_data$v12<-as.numeric(full_data$siyua_bediyur)*(-100)
  full_data$v13<-as.numeric(full_data$diyur_tizburi)*(-1)
  full_data$v14<-as.numeric(full_data$machalot_oni)*(-1)
  full_data$v15<-as.numeric(full_data$aliya)
  n_row<-as.numeric(nrow(full_data))
  
  #####׳—׳׳•׳§׳× ׳›׳ ׳׳©׳×׳ ׳” ׳׳¢׳©׳™׳¨׳•׳ ׳™׳ ׳׳₪׳™ ׳˜׳•׳•׳—
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
  
  #####׳—׳׳•׳§׳× ׳›׳ ׳׳©׳×׳ ׳” ׳׳¢׳©׳™׳¨׳•׳ ׳™׳ ׳׳₪׳™ ׳׳—׳•׳–׳•׳ ׳™׳  
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
  
  ####### ׳׳”׳›׳ ׳™׳¡ ׳₪׳” ׳׳•׳’׳™׳§׳” ׳©׳ ׳—׳׳•׳§׳× ׳¦׳™׳•׳ ׳™׳
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
    
    ####׳”׳₪׳¢׳׳× ׳”׳¡׳™׳ ׳•׳ ׳™׳  
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$residents_yeshuv>=input$residents[1] & data_for_heatmap$residents_yeshuv<=input$residents[2],]
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$migzar%in%input$migzar,]
    data_for_heatmap<<-data_for_heatmap[data_for_heatmap$year_bniya>=input$age_se[1] & data_for_heatmap$year_bniya<=input$age_se[2],]
    
  })
  
  corr_vars<-reactive({
    lapply(1:15,function(i)
    { if(input[["v1"]]==choice_names[i]) {p1<<-i}
      if(input[["v2"]]==choice_names[i]) {p2<<-i}})
     round(cor(full_data[,8+p1],full_data[,8+p2],use = "all.obs"),3)})
  
  
    output$corr_between_vars<-renderText(corr_vars())
  
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
  
  
  
  
  ####׳³ֵ¾׳³ֲ§׳³ֲ¨׳³ֲ ׳³ֵ“׳³ג€™׳³ג€¢׳³ג€׳³ֵ“ ׳³ֲ¨׳³ג€׳³ג„¢׳³ג€¢׳³ֲ¡
  addLegendCustom <- function(map,colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, title="מספר תושבים באזור סטטיסטי",opacity = opacity))}
  
  legend_path<-"www/combined_palette_map.jpg"

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
  


  ###׳³ֲ§׳³ג€¢׳³ג€˜׳³ֲ¥ ׳³ג€׳³ג„¢׳³ ׳³ֵ¾׳³ג„¢ ׳³ֵ“׳³ֲ¦׳³ג€¢׳³ֲ¨׳³ֵ¡ ׳³ג€׳³ג€׳³ג€¢׳³ֲ¨׳³ג€׳³ג€
  data_for_download<- reactive({
    calc_method()
    updatedData()
    if (grade_for_map=="total_grade_cat1")
    {data_for_file<<-data_for_heatmap[order(data_for_heatmap$total_grade_cat1),]}
    else
    {data_for_file<<-data_for_heatmap[order(data_for_heatmap$total_grade_cat2),]}
    base_col<-c(1,3,6,8,25)
    ###׳¢׳׳•׳“׳•׳× ׳”׳׳©׳×׳ ׳™׳ ׳©׳ ׳‘׳—׳¨׳•
    p_col<-c(9:23)
    c_p_col<-c(p_col[p1],p_col[p2])
    ####׳¢׳׳•׳“׳•׳× ׳™׳™׳¦׳•׳ ׳¢׳₪׳™ ׳©׳™׳˜׳× ׳”׳—׳™׳©׳•׳‘
    if (grade_for_map=="total_grade_cat1") {col_bin<-c(41+p1,41+p2)}
    else {col_bin<-c(56+p1,56+p2)}
    #####׳›׳׳ ׳”׳¢׳׳•׳“׳•׳×
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

  ####׳₪׳׳˜׳” ׳©׳ 4 ׳¦׳‘׳¢׳™׳
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




