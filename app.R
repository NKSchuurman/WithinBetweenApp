library(shiny)

#########################################################
########################################################
# Define UI for Within-Between app ----
ui <- fluidPage(
  
  titlePanel("Within vs Between vs Cross-sectional"),
  
  fluidRow(
    
    column(2,
           wellPanel(
    
    radioButtons(inputId="ICCornot", label=NULL, choices=list("ICC", "Std. Dev."),inline=TRUE), 
    
    sliderInput(inputId="rho_w", label="within rho", min=-1, max=1, value=-0.5,step=.1),
    sliderInput(inputId="rho_b", label="between rho", min=-1, max=1, value=0.5,step=.1),
    
    conditionalPanel(
      condition = "input.ICCornot == 'ICC'",
      # Input: ICCs
      sliderInput(inputId="ICCX", label="ICC X", min=0, max=1, value=0.5,step=.1),
      sliderInput(inputId="ICCY", label="ICC Y", min=0, max=1, value=0.5,step=.1)
      ),
    
    conditionalPanel(
      condition = "input.ICCornot == 'Std. Dev.'",
      # Input: Between sds
      sliderInput(inputId="sdX_b", label="between sd X", min=0, max=10, value=1,step=.1),
      sliderInput(inputId="sdY_b", label="between sd Y", min=0, max=10, value=1,step=.1),
       #Input: Within sds
      sliderInput(inputId="sdX_w", label="within sd X", min=0, max=10, value=1,step=.1),
      sliderInput(inputId="sdY_w", label="within sd Y", min=0, max=10, value=1,step=.1)
    ),
   
    #Input: Plotting Options
    sliderInput(inputId="np", label="N for plot", min=100, max=1000, value=500,step=100)
    
           )       
    ),
  
  # for displaying outputs 
  column(10,
         fluidRow(
           column(6, 
                  plotOutput("plot_b"),
                  plotOutput("plot_c")
                  ),
           
           column(6,
                  plotOutput("plot_w"))
 
 
        )
    )
  )
)

################################################################
################################################################
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  ###general user input dependent operations
  library(MASS)
  col_b = "#FF1545"
  col_w = "#00CFFF"
  col_c = c("#9807FF", "#9F01B3", "#4C18EA")
  
 ###Reactive functions
  
  ######for when input$ICCornot=="Std. Dev.######
  
    #Calculate grand standard deviation for X
 fsdX <- reactive({
    varX_b=input$sdX_b^2
    varX_w = input$sdX_w^2
    varX = varX_b + varX_w
    sdX  = sqrt(varX)
    return(sdX)
  })
  #Calculate grand standard deviation for Y
 fsdY <- reactive({
   varY_b = input$sdY_b^2 
   varY_w = input$sdY_w^2
   varY = varY_b + varY_w
   sdY  = sqrt(varY)
   return(sdY)
 })
 
##########################################
   
    frho_c <- reactive({
      
      if(input$ICCornot=="ICC"){
      rho_c = sqrt(input$ICCX) * sqrt(input$ICCY) * input$rho_b + sqrt(1-input$ICCX) * sqrt(1-input$ICCY) * input$rho_w
      return(rho_c)
      }
      if(input$ICCornot=="Std. Dev."){
        rho_c = input$sdX_b/fsdX() * input$sdY_b/fsdY() * input$rho_b + input$sdX_w/fsdX() * input$sdY_w/fsdY() * input$rho_w
        return(rho_c)
      }
      
   })

 fsimdat_b <- reactive({
   simdat_b = mvrnorm(n=input$np,mu=c(0,0), Sigma=matrix(c(1,input$rho_b,input$rho_b,1),2,2))
   return(simdat_b)
 })
 
 fsimdat_w <- reactive({
   simdat_w = mvrnorm(n=input$np,mu=c(0,0), Sigma=matrix(c(1,input$rho_w,input$rho_w,1),2,2))
   return(simdat_w)
 })
 
 fsimdat_c <- reactive({
   rho_c = frho_c()
   simdat_c = mvrnorm(n=input$np,mu=c(0,0), Sigma=matrix(c(1,rho_c,rho_c,1),2,2))
   return(simdat_c)
 })
 
 fcol_c_select <- reactive({
 col_c_roul=c(((input$rho_w+input$rho_b)/2- frho_c()), (input$rho_b-frho_c()), (input$rho_w-frho_c()))
 col_c_select = which(abs(col_c_roul)==min(abs(col_c_roul)))
 return(col_c_select)
 })
 
 ###Output 
 
 #Between plot
  output$plot_b <- renderPlot({
    plot_b=plot(fsimdat_b(),pch=19, ylim=c(-3,3),xlim=c(-3,3),col=col_b, xlab="X between (standardized)", ylab="Y between (standardized)", main=paste("Between rho =", input$rho_b))
  })
 
  #Within plot 
  output$plot_w <- renderPlot({
    plot_w=plot(fsimdat_w(),pch=19, ylim=c(-3,3),xlim=c(-3,3), col=col_w, xlab="X within (standardized)", ylab="Y within (standardized)", main=paste("Within rho =", input$rho_w) )
  })
  
  #Cross-sectional plot
  output$plot_c <- renderPlot({
    plot_c=plot(fsimdat_c(),pch=19, ylim=c(-3,3),xlim=c(-3,3),col=col_c[fcol_c_select()], xlab="X cross-sectional (standardized)", ylab="Y cross-sectional (standardized)", main=paste("Cross-sectional rho =", round(frho_c(),2)) )
  })
  
}

###############################################################
###############################################################
shinyApp(ui, server)