library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(wordcloud)
library(tm)
library(slam)

source("Global.R")

shiny::shinyApp(
  ui = argonDashPage(
    title = "Lefa",
    author = "Kenneth",
    description = "Analysis of Orders from Lefa",
    sidebar = argonDashSidebar(
      vertical = TRUE,
      skin = "light",
      background = "white",
      size = "md",
      side = "left",
      id = "my_sidebar",
      brand_url = "https://www.lefa.com.na/home",
      brand_logo = "https://www.lefa.com.na/api/v1/images/media/lefa/lefa-logo.png",
      # dropdownMenus = argonDropNav(
      #   title = "Dropdown Menu",
      #   src = "https://demos.creative-tim.com/argon-dashboard/assets/img/theme/team-4-800x800.jpg",
      #   orientation = "right",
      #   argonDropNavTitle(title = "Welcome!"),
      #   argonDropNavItem(
      #     title = "Item 1",
      #     src = "https://www.google.com",
      #     icon = "single-02"
      #   ),
      #   argonDropNavItem(
      #     title = "Item 2", 
      #     src = NULL, 
      #     icon = "settings-gear-65"
      #   ),
      #   argonDropNavDivider(),
      #   argonDropNavItem(
      #     title = "Item 3", 
      #     src = "#", 
      #     icon = "calendar-grid-58"
      #   )
      # ),
      argonSidebarHeader(title = "Main Menu"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "Dashboard",
          icon = argonIcon("tv-2"),
          icon_color = "primary",
          "Dashboard"
        ),
        argonSidebarItem(
          tabName = "User",
          icon = argonIcon("atom"),
          icon_color = "primary",
          "User Analytics"
        ),
        argonSidebarItem(
          tabName = "Driver",
          icon = argonIcon("atom"),
          icon_color = "primary",
          "Driver Analytics"
        ),
        argonSidebarItem(
          tabName = "Map",
          icon = argonIcon("map-big"),
          icon_color = "primary",
          "Map - Predictive"
        ),
        argonSidebarItem(
          tabName = "All",
          icon = argonIcon("map-big"),
          icon_color = "primary",
          "Map - All"
        ))),
    # navbar = argonDashNavbar(
    #   argonDropNav(
    #     title = "Dropdown Menu", 
    #     src = "https://demos.creative-tim.com/argon-dashboard/assets/img/theme/team-4-800x800.jpg", 
    #     orientation = "right",
    #     argonDropNavTitle(title = "Welcome!"),
    #     argonDropNavItem(
    #       title = "Item 1", 
    #       src = "https://www.google.com", 
    #       icon = "single-02"
    #     ),
    #     argonDropNavItem(
    #       title = "Item 2", 
    #       src = NULL, 
    #       icon = "settings-gear-65"
    #     ),
    #     argonDropNavDivider(),
    #     argonDropNavItem(
    #       title = "Item 3", 
    #       src = "#", 
    #       icon = "calendar-grid-58"
    #     )
    #   )
    # ),
    header = argonDashHeader(
      gradient = TRUE,
      color = "primary",
      separator = TRUE,
      separator_color = "secondary",
      argonCard(
        title = "Lefa Data Analytics",
        #src = "http://www.google.com",
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("sound-wave"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        "Plots and analysis derived from Lefa monthly data for February 2018 - February 2020"
      )
    ),
    body = argonDashBody(
      argonTabItems(
        argonTabItem(
          tabName = "Dashboard",
          argonRow(
            div(
            selectInput("allslct", "Select Month",
                        choices = c("ALL", as.character(sort(unique(condat$MonYear), decreasing = TRUE)))), style = "position: fixed; z-index: 1; width:8%; box-sizing:border-box; background-color: transparent;"),
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Orders per Day and Hour (Darker colurs indicate higher orders)",
              argonRow(
                  plotOutput("ordersperday")
              )
            )),
            br(), br(),
          argonRow(
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Order Outcomes",
              argonRow(
                argonColumn(
                  width = 4,
                  htmlOutput("pie1"))
              ,
                argonColumn(
                  width = 8,
                  htmlOutput("pie2"))
              ))),
            br(), br(),
          argonRow(
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Order Outcomes per District",
              argonRow(
                argonColumn(
                  width = 6,
                  plotlyOutput("DFC",width = "100%", height = "400"))
                ,
                argonColumn(
                  width = 6,
                  plotlyOutput("DCR",width = "100%", height = "400"))
              ))),
            br(), br(),
          argonRow(
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Order Outcomes per District (Pure Totals and Proportion of phone OS)",
              argonRow(
                argonColumn(
                  width = 6,
                  plotlyOutput("DPD",width = "100%", height = "400"))
                ,
                argonColumn(
                  width = 6,
                  plotlyOutput("DPB",width = "100%", height = "400"))
              ))),
            br(), br(),
          argonRow(
            argonCard(
              width = 12,
              title = "Orders Time Series",
              src = NULL,
              hover_lift = TRUE,
              shadow = TRUE,
              shadow_size = NULL,
              hover_shadow = FALSE,
              border_level = 0,
              #icon = "atom",
              status = "primary",
              background_color = NULL,
              gradient = FALSE, 
              floating = FALSE,
              argonRow(
                argonColumn(
                  width = 12,
                  plotlyOutput("annoplotly",width = "100%", height = "400")
                )
              )
            )),
          br(),
          argonRow(
            argonCard(
              width = 12,
              title = "Mean Ratings by Passengers Time Series",
              src = NULL,
              hover_lift = TRUE,
              shadow = TRUE,
              shadow_size = NULL,
              hover_shadow = FALSE,
              border_level = 0,
              #icon = "atom",
              status = "primary",
              background_color = NULL,
              gradient = FALSE, 
              floating = FALSE,
              argonRow(
                argonColumn(
                  width = 12,
                  plotlyOutput("ratingplotly",width = "100%", height = "400")
                )
              )
            )),
          br(),
          argonRow(
            argonInfoCard(
              value = textOutput("orders"), 
              title = "Total Orders", 
              stat = 5709, 
              stat_icon = argonIcon("arrow-up"),
              description = "In the last month", 
              icon = argonIcon("bell-55"), 
              icon_background = "default",
              hover_lift = TRUE,
              width = 3
            ),
             argonInfoCard(
               value = textOutput("cost"),
               title = "Avg Trip Cost",
               stat_icon = argonIcon("arrow-up"),
               icon = argonIcon("cart"),
               icon_background = "default",
               hover_lift = TRUE,
               shadow = TRUE,
               width = 3
             ),
             argonInfoCard(
               value = textOutput("dist"), 
               title = "Avg Trip Distance", 
               stat_icon = argonIcon("arrow-down"),
               icon = argonIcon("delivery-fast"), 
               icon_background = "default",
               hover_lift = TRUE,
               width = 3
             ),
             argonInfoCard(
               value = textOutput("time"), 
               title = "Avg Trip Time", 
               stat_icon = argonIcon("arrow-up"),
               icon = argonIcon("watch-time"), 
               icon_background = "default",
               gradient = TRUE,
               hover_lift = TRUE,
               width = 3
             )
        )),
        argonTabItem(
          tabName = "User",
              argonRow(div(
                selectInput("userslct", "Select Month",
                            choices = c("ALL", as.character(sort(unique(condat$MonYear), decreasing = TRUE)))), style = "position: fixed; z-index: 1; width:8%; box-sizing:border-box; background-color: transparent;"),
                argonCard(
                  width = 12,
                  src = NULL,
                  #icon = "ui-04",
                  status = "primary",
                  shadow = TRUE,
                  border_level = 2,
                  hover_shadow = TRUE,
                  hover_lift = TRUE,
                  title = "Super Spender",
                  argonRow(
                    argonColumn(
                      width = 4,
                      argonInfoCard(
                        value = uiOutput("ss"), 
                        icon = argonIcon("credit-card"), 
                        icon_background = "default",
                        gradient = TRUE,
                        hover_lift = TRUE,
                        width = 12
                      )
                      ),
                    argonColumn(
                      width = 8,
                      plotlyOutput("ssplotly",width = "100%", height = "400")
                    )
                  )
            )
          
         ),
           argonRow(
                 argonCard(
                   width = 12,
                   src = NULL,
                   #icon = "ui-04",
                   status = "primary",
                   shadow = TRUE,
                   border_level = 2,
                   hover_shadow = TRUE,
                   hover_lift = TRUE,
                   title = "Super User",
                   argonRow(
                     argonColumn(
                       width = 4,
                       argonInfoCard(
                         value = uiOutput("su"), 
                         icon = argonIcon("user-run"), 
                         icon_background = "default",
                         gradient = TRUE,
                         hover_lift = TRUE,
                         width = 12
                       )
                       ),
                       argonColumn(
                         width = 8,
                         plotlyOutput("suplotly",width = "100%", height = "400")
                       )
                     )
               )
             ),
         argonRow(
           argonCard(
             width = 6,
             src = NULL,
             #icon = "ui-04",
             status = "primary",
             shadow = TRUE,
             border_level = 2,
             hover_shadow = TRUE,
             hover_lift = TRUE,
             title = "Worst Rated User(s)",
             argonRow(
               argonColumn(
                 width = 12,
                 plotOutput("wr", width = "100%", height = "400px")
               )
             )
           ),
           argonCard(
             width = 6,
             src = NULL,
             #icon = "ui-04",
             status = "primary",
             shadow = TRUE,
             border_level = 2,
             hover_shadow = TRUE,
             hover_lift = TRUE,
             title = "Worst Rating User(s)",
             argonRow(
               argonColumn(
                 width = 12,
                 plotOutput("wrr", width = "100%", height = "400px")
               )
             )
           )
         )
         ),
        argonTabItem(
          tabName = "Driver",
          argonRow(
            div(
            selectInput("drvrslct", "Select Month",
                        choices = c("ALL", as.character(sort(unique(condat$MonYear), decreasing = TRUE)))), style = "position: fixed; z-index: 1; width:8%; box-sizing:border-box; background-color: transparent;"),
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Super Earner",
              argonRow(
                argonColumn(
                  width = 4,
                  argonInfoCard(
                    value = uiOutput("se"), 
                    icon = argonIcon("credit-card"), 
                    icon_background = "default",
                    gradient = TRUE,
                    hover_lift = TRUE,
                    width = 12
                  )
                ),
                argonColumn(
                  width = 8,
                  plotlyOutput("seplotly",width = "100%", height = "400")
                )
              )
            )
            
          ),
          argonRow(
            argonCard(
              width = 12,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Super Driver",
              argonRow(
                argonColumn(
                  width = 4,
                  argonInfoCard(
                    value = uiOutput("sd"), 
                    icon = argonIcon("delivery-fast"), 
                    icon_background = "default",
                    gradient = TRUE,
                    hover_lift = TRUE,
                    width = 12
                  )
                ),
                argonColumn(
                  width = 8,
                  plotlyOutput("sdplotly",width = "100%", height = "400")
                )
              )
            )
          ),
          argonRow(
            argonCard(
              width = 6,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Worst Rated Driver(s)",
              argonRow(
                argonColumn(
                  width = 12,
                  plotOutput("wrd", width = "100%", height = "400px")
                )
              )
            ),
            argonCard(
              width = 6,
              src = NULL,
              #icon = "ui-04",
              status = "primary",
              shadow = TRUE,
              border_level = 2,
              hover_shadow = TRUE,
              hover_lift = TRUE,
              title = "Worst Rating Driver(s)",
              argonRow(
                argonColumn(
                  width = 12,
                  plotOutput("wrrd", width = "100%", height = "400px")
                )
              )
            )
          )
        ),
        
        argonTabItem(
          tabName = "Map",
          argonRow(
            leafletOutput("heatmap", width="100%",height="1000px"),
            absolutePanel(top = 450,
                          left = 50,
                          draggable = F,
                          width=140,
                          height='auto',
                          dateInput('slt_date',label=NULL,
                                    value = Sys.Date(), min = '2018-08-01'),
                          uiOutput('hour_output')
            )
          )
        ),
        argonTabItem(
          tabName = "All",
          argonRow(
            leafletOutput("heatmapc", width="100%",height="1000px"),
            absolutePanel(top = 450,
                          left = 50,
                          draggable = F,
                          width=140,
                          height='auto'
            )
          )
        )
      )
    ),
    footer = argonDashFooter(
      copyrights = "@ergoanalytics, 2019",
      src = "https://github.com/DivadNojnarg",
      argonFooterMenu(
        argonFooterItem(
          tags$a(
          style="text-decoration:none",
          "kennethmwandingi@ergoanalysticscc.com",
          target = "_blank",
          class = "sg",
          href = "mailto:kennethmwandingi@ergoanalyticscc.com"
          )
        )
      )
    )
  ),
  server=function(input, output) {
    
          condatr <- reactive({
            
            if (input$allslct == "ALL") {
              
              v <- condat
              
            } else {
              v <- subset(condat, MonYear == input$allslct)
              v <- droplevels(v)
              
            }

            return(v)
          })
          
          condatru <- reactive({
            
            if (input$userslct == "ALL") {
              
              v <- condat
              
            } else {
              v <- subset(condat, MonYear == input$userslct)
              v <- droplevels(v)
              
            }
            
            return(v)
          })
          
          condatrd <- reactive({
            
            if (input$drvrslct == "ALL") {
              
              v <- condat
              
            } else {
              v <- subset(condat, MonYear == input$drvrslct)
              v <- droplevels(v)
              
            }
            
            return(v)
          })

          dos <- reactive({
            
            if (input$allslct == "ALL") {
              
              v <- distorderstat
              
            } else {
              
              v <- subset(distorderstat, MonYear == input$allslct)
              
              v <- droplevels(v)
              
            }
            
            return(v)
          })
          
          output$orders <- renderText({
            
            c <- condatr()
            
            orders <- as.character(as.integer(count(c)))
            
            orders <- paste(orders, "Orders")
            
          })
          
          output$cost <- renderText({
            
            c <- condatr()
            
            AvgCost <- as.character(round(mean(c$`final cost, NAD`, na.rm = T),2))
            
            AvgCost <- paste(AvgCost, "NAD")
            
          })
          
          output$dist <- renderText({
            
            c <- condatr()
            
            AvgDist <- as.character(round(mean(as.numeric(gsub('.{2}$', '', c$`trip distance`)), na.rm = T),2))
            
            AvgDist <- paste(AvgDist, "KM")
            
            
          })
          
          output$time <- renderText({
            
            c <- condatr()
            
            Avgtime <- gsub("m", ":", c$`trip time`)
            Avgtime <- gsub("s", "", Avgtime)
            Avgtime <- gsub("h", ":", Avgtime)
            Avgtime <- gsub("min", ":00", Avgtime)
            Avgtime <- gsub(" ", "", Avgtime)
            Avgtime <- sapply(strsplit(Avgtime,":"), function(x) {
              x <- as.numeric(x)
              x[1]+x[2]/60
            })
            
            Avgtime <- as.character(round(mean(Avgtime, na.rm = T), 2))
            
            Avgtime <- paste(Avgtime, "Minutes")
            
            
          })
          
          output$ss <-renderUI({
            
            c <- condatru()
            
            hc <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger, phone, email) %>%
                                  dplyr::summarise(Spend = sum(cost, na.rm = TRUE)))
            
            ss <- subset(hc, Spend == max(hc$Spend, na.rm = TRUE))
            
            #ss <- paste(ss$passenger, ss$phone, ss$email, sep = "\n")
            
            HTML(paste0(ss$passenger, "<br>", ss$phone,"<br>", ss$email, "<br><br>Spent: ", "N$ ", ss$Spend))
            
            
          })
          
          output$su <- renderUI({
            
            c <- condatru()
            
            su <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger, phone, email) %>%
                                  dplyr::summarise(N = length(passenger)))
            
            sui <- subset(su, N == max(su$N, na.rm = TRUE))
            
            HTML(paste0(sui$passenger, "<br>", sui$phone,"<br>", sui$email, "<br><br>Orders: ", sui$N))
            
            
          })
          
          output$se <-renderUI({
            
            c <- condatrd()
            
            hc <- as.data.frame(c %>% 
                                  dplyr::group_by(driver, phone, email) %>%
                                  dplyr::summarise(Spend = sum(cost, na.rm = TRUE)))
            
            hc <- na.omit(hc)
            
            ss <- subset(hc, Spend == max(hc$Spend, na.rm = TRUE))
            
            #ss <- paste(ss$passenger, ss$phone, ss$email, sep = "\n")
            
            HTML(paste0(ss$driver, "<br>", ss$phone,"<br>", ss$email, "<br><br>Spent: ", "N$ ", ss$Spend))
            
            
          })
          
          output$sd <- renderUI({
            
            c <- condatrd()
            
            su <- as.data.frame(c %>% 
                                  dplyr::group_by(driver, phone, email) %>%
                                  dplyr::summarise(N = length(driver)))
            
            su <- na.omit(su)
            
            sui <- subset(su, N == max(su$N, na.rm = TRUE))
            
            HTML(paste0(sui$driver, "<br>", sui$phone,"<br>", sui$email, "<br><br>Orders: ", sui$N))
            
            
          })

          output$wr <- renderPlot({
            
            c <- condatru()
            
            wr <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger, phone, email) %>%
                                  dplyr::summarise(Rating = mean(ratingbydriver, na.rm = TRUE)))
            
            wri <- subset(wr, Rating == min(wr$Rating, na.rm = TRUE))
            
            
            if (count(wri)!=0){
              
              wordcloud(words = wri$passenger, scale=c(2,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            } else {
              
              wordcloud(words = c("No","Data"), scale=c(4,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            }
            
            
          })
          
          output$wrr <- renderPlot({
            
            c <- condatru()
            
            wr <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger, phone, email) %>%
                                  dplyr::summarise(Rating = mean(ratingbypass, na.rm = TRUE)))
            
            wri <- subset(wr, Rating == min(wr$Rating, na.rm = TRUE))
            
            if (count(wri)!=0){
              
              wordcloud(words = wri$passenger, scale=c(2,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
           
               } else {
              
                 wordcloud(words = c("No","Data"), scale=c(2,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
                 
              }
            
            
          })
          
          output$wrd <- renderPlot({
            
            c <- condatrd()
            
            wr <- as.data.frame(c %>% 
                                  dplyr::group_by(driver, phone, email) %>%
                                  dplyr::summarise(Rating = mean(ratingbydriver, na.rm = TRUE)))
            
            wri <- subset(wr, Rating == min(wr$Rating, na.rm = TRUE))
            
            
            if (count(wri)!=0){
              
              wordcloud(words = wri$driver, scale=c(6,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            } else {
              
              wordcloud(words = c("No","Data"), scale=c(6,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            }
            
          })
          
          output$wrrd <- renderPlot({
            
            c <- condatrd()
            
            wr <- as.data.frame(c %>% 
                                  dplyr::group_by(driver, phone, email) %>%
                                  dplyr::summarise(Rating = mean(ratingbypass, na.rm = TRUE)))
            
            wri <- subset(wr, Rating == min(wr$Rating, na.rm = TRUE))
            
            if (count(wri)!=0){
              
              wordcloud(words = wri$driver, scale=c(4,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            } else {
              
              wordcloud(words = c("No","Data"), scale=c(4,.5), min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
              
            }
            
            
          })

          output$ordersperday <- renderPlot({
            
            c <- condatr()
            
            col1 = "#bfddff" 
            col2 = "#6A5ACD"
            
            dayHour <- ddply(c, c( "hour", "wday", 'Constituency'), summarise,
                             N    = length(ymd), lat = mean(lat), long = mean(long)
            )
            
            dayHour$DayofWeek <- as.numeric(dayHour$wday)
            dayHour$DayofWeek <- dayHour$DayofWeek - 1
   
            ggplot(dayHour, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
              scale_fill_gradient(low = col1, high = col2) +  
              guides(fill=guide_legend(title="Total Orders")) +
              theme_bw() + theme_minimal() + 
              labs(x = "Hour", y = "") +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    plot.title = element_text(hjust=0.5))})
          
          output$pie1 <- renderGvis({
            
            c <- condatr()
            
            orderstat<-as.data.frame(table(unlist(c$`order status`)))
            names(orderstat)[names(orderstat) == 'Var1'] <- 'orderstat'
            cancres <- as.data.frame(table(unlist(c$`cancellation reason`)))
            
            Cancelled = grep(orderstat$orderstat, pattern = "CANCELLED")
            
            Finished_Paid = grep(orderstat$orderstat, pattern = "FINISHED_PAID")
            
            Finished_Unpaid = grep(orderstat$orderstat, pattern = "FINISHED_UNPAID")
            
            orderstatp = data.frame(orderstat = c("CANCELLED", "FINISHED_PAID", "FINISHED_UNPAID"), 
                                    Freq = c(sum(orderstat$Freq[Cancelled]), 
                                             sum(orderstat$Freq[Finished_Paid]), 
                                             sum(orderstat$Freq[Finished_Unpaid])))
            
            
            gvisPieChart(orderstatp, labelvar="orderstat", 
                                                  numvar="freq", options=list(width="500px", 
                                                  colors="['#CEA7F1', '#5e72e4', '#5e72e4','#713CA0', '#42235C', '#11cdef']"))})
          
          output$pie2 <- renderGvis({
            
            c <- condatr()
            
            orderstat<-as.data.frame(table(unlist(c$`order status`)))
            names(orderstat)[names(orderstat) == 'Var1'] <- 'orderstat'
            cancres <- as.data.frame(table(unlist(c$`cancellation reason`)))
            
            
            cancres %>% dplyr::arrange(desc(Freq)) -> cancres
            
            cancres <- cancres[1:6,]
              
            gvisPieChart(cancres,labelvar="Var1",
                         numvar = "Freq", options=list(width="700px",
                         colors="['#CEA7F1', '#92A0ED', '#5e72e4','#713CA0', '#42235C', '#11cdef']"))})

          output$annoplotly <- renderPlotly({
            
            c <- condatr()
            
            date <- ddply(c, c( "ymd"), summarise,
                          N    = length(ymd)
            )
            
            anotdates <- merge(date, Nam_Public_Holidays, by=c("ymd"),all=TRUE)
            #delete NA rows in ymd
            a <- anotdates[!(is.na(anotdates$ymd)),]
            
            plot_ly(a, y=~N, x=~ymd, mode='line + marker', 
                                                    type = 'scatter', text= anotdates$Holiday, color=I("royalblue"))%>%
              layout(yaxis = list(title = "Orders", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% config(displayModeBar = F)
          })
          
          output$ratingplotly <- renderPlotly({
            
            c <- condatr()
            
            date <- as.data.frame(c %>% dplyr::select(ymd, ratingbypass) %>% group_by(ymd) %>% dplyr::summarise(R = mean(ratingbypass, na.rm = TRUE), N = length(ymd)))
            
            a <- date[!(is.na(date$R)),]
            
            a$col <- ifelse(a$R>4.5,"green", ifelse(a$R>3.5,"yellow", "orange"))
            
            a$name <- ifelse(a$R>4.5,"Good", ifelse(a$R>3.5,"Concern", "Danger"))
            
            a$name <- factor(a$name, levels = c("Good", "Concern", "Danger"))
            

            plot_ly(a, y=~R, x=~ymd, mode='markers', 
                    type = 'scatter', text = a$N, color = a$col, name = a$name)%>%
              layout(yaxis = list(title = "Mean Rating", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% config(displayModeBar = F)
          })
          
          output$ssplotly <- renderPlotly({
            
            c <- condatru()
            
            hc <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger) %>%
                                  dplyr::summarise(Spend = sum(cost, na.rm = TRUE)))
            
            ss <- subset(hc, Spend == max(hc$Spend, na.rm = TRUE))
            
            ss <- as.character(ss$passenger)
            
            c <- subset(c, passenger == ss)
            
            a <- as.data.frame(c %>% dplyr::select(ymd) %>% group_by(ymd) %>% dplyr::summarise(N = length(ymd)))
            
            
            plot_ly(a, y=~N, x=~ymd, mode='markers', 
                    type = 'scatter', color=I("royalblue"))%>%
              layout(yaxis = list(title = "Orders", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% 
              config(displayModeBar = F)
          })
          
          output$suplotly <- renderPlotly({
            
            c <- condatru()
            
            su <- as.data.frame(c %>% 
                                  dplyr::group_by(passenger) %>%
                                  dplyr::summarise(N = length(passenger)))
            
            su <- na.omit(su)
            
            sui <- subset(su, N == max(su$N, na.rm = TRUE))
            
            sui <- as.character(sui$passenger)
            
            c <- subset(c, passenger == sui)
            
            a <- as.data.frame(c %>% dplyr::select(ymd) %>% group_by(ymd) %>% dplyr::summarise(N = length(ymd)))
            
            
            plot_ly(a, y=~N, x=~ymd, mode='markers', 
                    type = 'scatter', color=I("royalblue"))%>%
              layout(yaxis = list(title = "Orders", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% config(displayModeBar = F)
          })
          
          output$seplotly <- renderPlotly({
            
            c <- condatrd()
            
            hc <- as.data.frame(c %>% 
                                  dplyr::group_by(driver) %>%
                                  dplyr::summarise(Spend = sum(cost, na.rm = TRUE)))
            
            ss <- subset(hc, Spend == max(hc$Spend, na.rm = TRUE))
            
            ss <- na.omit(ss)
            
            ss <- as.character(ss$driver)
            
            c <- subset(c, driver == ss)
            
            a <- as.data.frame(c %>% dplyr::select(ymd) %>% group_by(ymd) %>% dplyr::summarise(N = length(ymd)))
            
            
            plot_ly(a, y=~N, x=~ymd, mode='markers', 
                    type = 'scatter', color=I("royalblue"))%>%
              layout(yaxis = list(title = "Orders", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% 
              config(displayModeBar = F)
          })
          
          output$sdplotly <- renderPlotly({
            
            c <- condatrd()
            
            su <- as.data.frame(c %>% 
                                  dplyr::group_by(driver) %>%
                                  dplyr::summarise(N = length(driver))) 
            su <- na.omit(su)
            
            sui <- subset(su, N == max(su$N, na.rm = TRUE))
           
            
            sui <- as.character(sui$driver)
            
            c <- subset(c, driver == sui)
            
            a <- as.data.frame(c %>% dplyr::select(ymd) %>% group_by(ymd) %>% dplyr::summarise(N = length(ymd)))
            
            
            plot_ly(a, y=~N, x=~ymd, mode='markers', 
                    type = 'scatter', color=I("royalblue"))%>%
              layout(yaxis = list(title = "Orders", size = 1, zeroline = F), xaxis = list(title = "", size = 1, zeroline = F)
              ) %>% config(displayModeBar = F)
          })

          output$heatmap <- renderLeaflet({
            
            
            # leaflet(data=O18) %>% 
            #   #addProviderTiles(providers$CartoDB.Positron)%>% # Add third-party basemap tiles
            #   addTiles() %>%  # Add default OpenStreetMap map tiles
            #   #addMarkers(lng=O18$long, lat=O18$lat, popup="The birthplace of R")%>%
            #   addCircleMarkers(
            #     color=~pal(hour))
            date_input <- strftime(input$slt_date,'%u')
            
            date_input <- as.numeric(date_input)-1
            
            count_tojoin <- TaxiPickData
            
            count_tojoin = count_tojoin %>%
              filter(DayofWeek == as.character(date_input)) %>%
              filter(Hour == input$hour) %>%
              group_by(Zipcode) %>%
              summarise(Total = sum(Count)) %>%
              mutate(Zipcode=factor(Zipcode))
            
            total <- count_tojoin$Total
            names(total) <- count_tojoin$Zipcode
            districtscount <- districts
            districtscount@data$Total <- total[as.character(districts@data$GID_2)]
            
            Avgloc = dayHour %>%
              filter(DayofWeek == as.character(date_input)) %>%
              filter(hour == input$hour) %>%
              group_by(Constituency) %>%
              summarise(lat = mean(lat), long = mean(long)) 
            
            
            leaflet(options = leafletOptions(minZoom = 12)) %>%
              # leaflet::addTiles() %>%
              addProviderTiles("CartoDB.Positron") %>% 
              addPolygons(data=districtscount,
                          fillColor = cols,
                          fillOpacity = 0.2,
                          color = "#BDBDC3",
                          stroke = TRUE,
                          weight = 2,
                          layerId = districtscount@data$GID_2,
                          popup = count_popup,
                          label = ~districtscount@data$Total,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, textsize = "20px")) %>%
              addCircleMarkers(data=Avgloc)%>%
              setView(lng = 17.08,lat = -22.55, zoom = 12)
              #%>%suspendScroll()
            })  

          output$heatmapc <- renderLeaflet({
            
            leaflet(data=O18, options = leafletOptions(minZoom = 12)) %>%
              #addProviderTiles(providers$CartoDB.Positron)%>% # Add third-party basemap tiles
              addTiles() %>%  # Add default OpenStreetMap map tiles
              #addMarkers(lng=O18$long, lat=O18$lat, popup="The birthplace of R")%>%
              addMarkers(
                clusterOptions = markerClusterOptions()
              )%>%
              setView(lng = 17.08,lat = -22.55, zoom = 12)
            #%>%suspendScroll()
          })

          output$DFC <- renderPlotly({
            
          distorderstat <- dos()
            
          distorderstat <- distorderstat %>%
                filter(!(bundle %in% ""))
              
          ggplotly(ggplot(
                distorderstat) +
                aes(x = Constituency, fill = order.status) +
                geom_bar(position = "fill") +
                scale_fill_manual(values=myPalette) +
                coord_flip() +
                theme_minimal() +
                labs(x = "", y = "Orders", fill = "")
            )%>% 
            config(displayModeBar = F)
          })

          output$DCR <- renderPlotly({
            
            distorderstat <- dos()
            
            distorderstat <- distorderstat %>%
              filter(!is.na(cancellation.reason)) %>%
              filter(!(bundle %in% ""))
            
            ggplotly(
              ggplot(distorderstat) +
                       aes(x = Constituency, fill = cancellation.reason) +
                       geom_bar(position = "fill") +
                       scale_fill_manual(values=myPalette) +
                       coord_flip() +
                       theme_minimal() +
                labs(x = "", y = "Orders", fill = "")
            )%>% 
              config(displayModeBar = F)
          })
          
          output$DPD <- renderPlotly({
            
            ggplotly(
              ggplot(TaxiPickData) +
                       aes(x = Neighborhood, fill = wday, weight = Count) +
                       geom_bar() +
                       scale_fill_manual(values=myPalette)  +
                       coord_flip() +
                       theme_minimal() +
                       labs(x = "", y = "Orders", fill = "")
            )%>% 
              config(displayModeBar = F)
          })
          
          output$DPB <- renderPlotly({
            
            distorderstat <- dos()
            
            distorderstat <- distorderstat %>%
              filter(bundle %in% c("android", "ios", "web.dispatch"))
            
            ggplotly(ggplot(
              distorderstat) +
                aes(x = Constituency, fill = bundle) +
                geom_bar(position = "fill") +
                scale_fill_manual(values=myPalette) +
                coord_flip() +
                theme_minimal() +
                labs(x = "", y = "Orders", fill = "")
            )%>% 
              config(displayModeBar = F)
          })
          
          output$hour_output <- renderUI({
            
            sliderInput("hour", "Hour",
                        min=0, max=23, value=0)
            
          })
          
  }
  )