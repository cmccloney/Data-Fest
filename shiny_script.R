well <- read.csv("wellness.csv")
rpe <- read.csv("rpe.csv")
games <- read.csv("games.csv")

library(forecast)
library(ggplot2)
library(readr)
library(plyr)
library(dplyr)
library(nlme)
library(car)
library(MuMIn)
library(mice)
library(psych)
library(effects)
library(predictmeans)

wellness <- well
x <- wellness %>% group_by(Date,PlayerID) %>% select(Fatigue, Soreness, Desire, Irritability, SleepHours, SleepQuality, Pain, Illness, Menstruation, USG, TrainingReadiness)
y <- rpe
z <- join(y,x,by=NULL, type="left", match="all")
z <- z %>% filter(!Training == 'No')
z <- z[, -c(7:14)] 
z <- z[, -c(16)]
z <- z[, -c(3)]
z <- z %>% filter(!is.na(RPE))
z$TrainingReadiness <- as.numeric(sub("%", "", z$TrainingReadiness))*.01
z$Pain <- as.factor(z$Pain)
z$Illness <- as.factor(z$Illness)
z$Menstruation <- as.factor(z$Menstruation)
z$SessionType <- as.factor(z$SessionType)

test <- rpe %>% group_by(Date, PlayerID) %>% summarize(mean_size = mean(RPE, na.rm = TRUE))
test2 <- join(well, test, by = NULL, type = "left", match = "all")
library(nlme)
test2$PlayerID <- as.factor(test2$PlayerID)
test3 <- test2 %>% filter(!is.na(mean_size))
test3$PlayerID <- as.factor(test3$PlayerID)
##############
library(shiny)
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Player",
                  label = "Pick a player:",
                  min = 1,
                  max = 17,
                  value = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "plot2")
      ,plotOutput(outputId = "plot3")
      ,plotOutput(outputId = "plot1")
      ,plotOutput(outputId = "plot4")
      
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    rpe2 <- test3[test3$PlayerID == input$Player,]
    color.value <- ((0x11111111*input$Player) %% 0x111111) + 0x01010101
    if(color.value == 16843145){
      color.value <- 0x00000000
    }
    par(mfrow=c(2,1))
    par(mar=c(4.0,4.5,0.5,0.5))
    boxplot(rpe2$mean_size~rpe2$Pain,xlab="Pain",ylab="RPE", col = as.hexmode(color.value))
    boxplot(rpe2$mean_size~rpe2$Desire,xlab="Desire",ylab="RPE", col = as.hexmode(color.value))
  })
  
  output$plot4 <- renderPlot({
    rpe2 <- test3[test3$PlayerID == input$Player,]
    color.value <- ((0x11111111*input$Player) %% 0x111111) + 0x01010101
    if(color.value == 16843145){
      color.value <- 0x00000000
    }
    par(mfrow=c(2,1))
    par(mar=c(4.0,4.5,0.5,0.5))
    boxplot(rpe2$mean_size~rpe2$Fatigue,xlab="Fatigue",ylab="RPE", col = as.hexmode(color.value))
    boxplot(rpe2$mean_size~rpe2$Irritability,xlab="Irritability",ylab="RPE", col = as.hexmode(color.value))
  })
  
  output$plot2 <- renderPlot({
    rpe2 <- rpe[rpe$PlayerID == input$Player,]
    color.value <- ((0x11111111*input$Player) %% 0x111111) + 0x01010101
    sload.ts <- ts(na.omit(rpe2$AcuteLoad),frequency=4)
    sload.ts <- na.interp(sload.ts)
    sload.ts[] <- rev(sload.ts)
    mod1 <- auto.arima(sload.ts)
    cast <- forecast(mod1,h = 10)
    m <- max(na.omit(rpe2$AcuteLoad))
    plot(cast,col=as.hexmode(color.value),xlab="Time (by Week)",ylab="Acute Load",ylim=c(0,m),main=paste("Acute Load over Time for Player",input$Player, "plus Forecasting"))
  })
  
  output$plot3 <- renderPlot({
    rpe2 <- rpe[rpe$PlayerID == input$Player,]
    color.value <- ((0x11111111*input$Player) %% 0x111111) + 0x01010101
    ACratio.ts <- ts(na.omit(rpe2$AcuteChronicRatio),frequency=4)
    ACratio.ts <- na.interp(ACratio.ts)
    ACratio.ts[] <- rev(ACratio.ts)
    mod2 <- auto.arima(ACratio.ts)
    cast2 <- forecast(mod2,h = 10)
    m <- max(na.omit(rpe2$AcuteChronicRatio))
    plot(cast2,col=as.hexmode(color.value),xlab="Time (by Week)",ylab="Acute Chronic Ratio",ylim=c(0,m),main=paste("Acute Chronic Ratio over Time for Player",input$Player, "plus Forecasting"))
  })
  
}

shinyApp(ui = ui, server = server)
