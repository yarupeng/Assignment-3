#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# data clean
data2010<-read.csv("bp appre 2010.csv")
data2010
data2017<-read.csv("bp appre 2017.csv")
datas<-read.csv("BP monthly summaries.csv")
datas2 = apply(t(datas[,2:13]),1,rev) 
datas$mean<-apply(datas[,2:13],1,mean)
ndata<-nrow(datas[,2:13])
rownames(datas)<-datas[,1]
datas<-cbind(datas,rowSums(datas))
colnames(datas)<-c(colnames(datas)[-length(colnames(datas))],"Total")
datas<-subset(datas,select=-c(year))
rownames(data2010)<-data2010[,1]
data2010<-subset(data2010,select=-c(Sector))
rownames(data2017)<-data2017[,1]
data2017<-subset(data2017,select=-c(Sector))
data2010 <- rbind(data2010, colSums(data2010))
rownames(data2010) <- c(rownames(data2010)[-length(rownames(data2010))], "Total")
data2010 <- cbind(data2010,rowSums(data2010))
colnames(data2010) <- c(colnames(data2010)[-length(colnames(data2010))], "Total")
data2017 <- rbind(data2017, colSums(data2010))
rownames(data2017) <- c(rownames(data2017)[-length(rownames(data2017))], "Total")
data2017 <- cbind(data2017,rowSums(data2017))
colnames(data2017) <- c(colnames(data2017)[-length(colnames(data2017))], "Total")

#stats data
mean2010s<-apply(data2010[,1:12],1,mean)
mean2010m<-apply(data2010[,1:12],2,mean)
mean2017s<-apply(data2017[,1:12],1,mean)
mean2017m<-apply(data2017[,1:12],2,mean)
bpsector<-cbind(mean2010s,mean2017s)
bpmonth<-cbind(mean2010m,mean2017m)
sec2010<-as.data.frame(t(data2010[,1:12]))
as.character(colnames(sec2010))
sec2017<-as.data.frame(t(data2017[,1:12]))
as.character(colnames(sec2017))
t.test(sec2010$Tucson,sec2017$`Rio Grande Valley`)
month2010<-as.data.frame((data2010[,6:8]))
as.character(rownames(month2010))
month2010$sum<-apply(month2010,1,sum)
maxmonth2010<-max(month2010$sum)
month2017<-as.data.frame((data2017[,1:3]))
as.character(rownames(month2017))
month2017$sum<-apply(month2017,1,sum)
maxmonth2017<-max(month2017$sum)
ttestOut = t.test(month2010$sum,month2017$sum)
print(ttestOut)

ts2 <- as.vector(t(datas2))
ts3 <- ts(ts2,frequency = 13,start = c(2000,10))
datasmean <- apply(datas2,1,mean)
library(shiny)
#library(ggplot2)
# Define UI for application that draws a histogram
options(shiny.sanitize.errors = FALSE)
ui <- fluidPage(
  titlePanel("Project 1 plots"),
  sidebarLayout(position = "left",
                sidebarPanel(checkboxInput("comparebysector", 
                                           "compare by sector", 
                                           value = T),
                             checkboxInput("comparebymonth", 
                                           "compare by month", 
                                           value = T)
                ),
                mainPanel(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), 
                                        plotOutput("plotgraph1"), 
                                        plotOutput("plotgraph2"))
                            #plotOutput("plotgraph3"),
                            #plotOutput("plotgraph4"))
                          ),
                          tabsetPanel(
                            
                            tabPanel("Compare by sector", plotOutput("plotgraph3")),
                            tabPanel("Compare by month", plotOutput("plotgraph4")),
                            tabPanel("time series", plotOutput("plotgraph5")),
                            tabPanel("T-Test", textOutput("ttestOut")),
                            
                            p(),
                            p("We did a Waltch 2 Sample t test on the 3 month periods with the most apprehensions in 2010 and 2017. The 3 month periods with the most apprehensions in 2010 is March, April, and May. The 3 month periods with the most apprehensions in 2017 is October, November, and December. The p-value we got for this t-test is 0.9788. Since the p-value is way larger than 5% significance level, we fail to reject the null hypothesis and conclude that there is no significant change in the maximum of the 3 consecutive monthly apprehensions. "),
                            
                            p("From the bar plots, we can see that the sector with most apprehensions for 2010 is Tucson with total number of 212202 apprehensions. The sector with the most apprehensions for 2017 is Rio Grand Valley with total number of 137562 apprehensions. So, we use Waltch 2 Sample t test using these data. The p-value we got for this t-test is 0.06346. Since the p-value is larger than 5% significance level, we fail to reject the null hypothesis and conclude that there is no significant change in the maximum.")
                          )
                          
                )
                
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(1234)
  pt3 <- reactive({
    #print(input$do2)
    if(input$comparebysector){
      return(barplot(t(bpsector),
                     beside = TRUE,
                     legend.text = c("2010","2017"),
                     main = "compare by sector"))
    }
    else{
      return(NULL)
    }
  })
  
  pt4 <- reactive({
    if(input$comparebymonth){
      return(barplot(t(bpmonth),
                     beside = TRUE,
                     legend.text = c("2010","2017"),
                     main = "compare by month"))
    }
    else{
      return(NULL)
    }
  })
  
  output$plotgraph1 = renderPlot({
    barplot(data2017[1:9,13], names.arg = rownames(data2017)[1:9], 
            las=2,
            axisnames=TRUE,
            main="2017 Border Patrol Apprehensions by Sector",
            border="blue",
            col="red")
  })
  output$plotgraph2 = renderPlot({
    barplot(data2010[1:9,13], names.arg = rownames(data2010)[1:9], 
            las=2,
            axisnames=TRUE,
            main="2010 Border Patrol Apprehensions by Sector",
            border="blue",
            col="yellow")
  })
  output$plotgraph5 = renderPlot({
    ts.plot(ts3,gpars = list(xlab="year",ylab="appre"))
    points(c(2001:2018), datasmean, pch = 19, col = 'red')
    text(c(2001:2018), datasmean, c(2000:2017))
  })
  
  output$plotgraph3 = renderPlot({pt3()})
  output$plotgraph4 = renderPlot({pt4()})
  
  output$ttestOut = renderPrint({
    return(t.test(month2010$sum,month2017$sum))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

