library(shiny)

#########################################################
########################################################
# Define UI for Within-Between app ----
ui <- fluidPage(
  
  navbarPage("Within vs Between vs Cross-sectional",
    tabPanel("Pairwise Correlations",
            
    #titlePanel("Within vs Between vs Cross-sectional"),
  
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
   ),

    tabPanel("Univariate Distributions",
    
             fluidRow(
               
               column(2,
                      wellPanel(
                        
                        radioButtons(inputId="dist_ICCornot", label=NULL, choices=list("ICC", "Variance"),inline=TRUE), 
                        
                       
                        conditionalPanel(
                          condition = "input.dist_ICCornot == 'ICC'",
                          # Input: ICCs
                          sliderInput(inputId="dist_ICC", label="ICC", min=0, max=1, value=0.8,step=.1)
                        ),
                        
                        conditionalPanel(
                          condition = "input.dist_ICCornot == 'Variance'",
                          sliderInput(inputId="dist_var_w", label="Average Within-Person Variance", min=0, max=10, value=1,step=.5),
                          sliderInput(inputId="dist_var_b", label="Between Person Variance", min=0, max=10, value=5,step=.5)
                        )
                        
                       
                      )       
               ),
               
             #   for displaying outputs 
              column(10,
                     fluidRow(
                        column(6, 
                               plotOutput("dist_plot_b"),
                              plotOutput("dist_plot_c")
                        ),
                        
                       column(6,
                               plotOutput("dist_plot_w"))
                        
                        
                     )
               )
          )
    )#,         
    
    #tabPanel("Component 3")
  )
)
################################################################
################################################################
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
###general user input dependent operations###
#############################################  
  library(MASS)
  col_b = "#FF1545"
  col_w = "#00CFFF"
  col_c = c("#9807FF", "#9F01B3", "#4C18EA")
  
###dist###
  dist_grand_mean= 0
  dist_bp_mean=dist_grand_mean
  dist_av_wp_mean=dist_grand_mean
  
########Reactive functions##########
###################################  
  
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
 
#######Dist########
 fdist_var_w <- reactive({ 
    if(input$dist_ICCornot=="ICC"){
      dist_av_wp_var=(1-input$dist_ICC)*1
    }
   if(input$dist_ICCornot=="Variance"){
     dist_av_wp_var=input$dist_var_w
   }
   return(dist_av_wp_var)   
 })
 
 fdist_var_b <- reactive({ 
   if(input$dist_ICCornot=="ICC"){
     dist_bp_var= input$dist_ICC*1
   }
   if(input$dist_ICCornot=="Variance"){
     dist_bp_var= input$dist_var_b
   }
   return(dist_bp_var)   
 })
 
 fdist_grand_var <- reactive({ 
        dist_grand_var= fdist_var_w()+fdist_var_b() 
    return(dist_grand_var)   
 })
 
 fdist_x_avwithin <- reactive({
     x_avwithin <- seq(-4,4,length=300)*sqrt(fdist_var_w()) + dist_av_wp_mean
 return(x_avwithin)
 })
 
 fdist_hx_avwithin <- reactive({
   if(fdist_var_w()==0){
   dist_hx_avwithin <- rep(0,300)
   } else { dist_hx_avwithin <- dnorm(fdist_x_avwithin(),dist_av_wp_mean,sqrt(fdist_var_w())) } ###av_wp_var =/= av_wp_sd. So we have to assume that everyone hase the same within person variance to make this work.
   return(dist_hx_avwithin)
 })
 
 fdist_x_bp <- reactive({
   x_bp <- seq(-4,4,length=300)*sqrt(fdist_var_b()) + dist_bp_mean
     return(x_bp)
 })
 
 fdist_hx_bp <- reactive({
   if(fdist_var_b()==0){
     dist_hx_bp <- rep(0,300)
   } else {
   dist_hx_bp <- dnorm(fdist_x_bp(),dist_bp_mean,sqrt(fdist_var_b()))}
   return(dist_hx_bp)
 })
 

 fdist_x_grand <- reactive({
   x_grand <- seq(-4,4,length=300)*sqrt(fdist_grand_var()) + dist_grand_mean
     return(x_grand)
 })
 
 fdist_hx_grand <- reactive({
   if(fdist_grand_var()==0){
     dist_hx_grand <- rep(0,300)
   } else {
   dist_hx_grand <- dnorm( fdist_x_grand(),dist_grand_mean,sqrt(fdist_grand_var()))}
   return(dist_hx_grand)
 })

 fdist_xlim <- reactive({
 dist_xlim <- c((-4*sqrt( fdist_grand_var())+dist_grand_mean),(4*sqrt( fdist_grand_var())+dist_grand_mean))
 return(dist_xlim)
})
 
 fdist_ylim <- reactive({
   minvar= fdist_var_b()==min(c(fdist_var_b(),fdist_var_w()))
     if(minvar==TRUE){
     dist_ylim= c(0,max(fdist_hx_bp()))
   } else{
     dist_ylim= c(0,max(fdist_hx_avwithin()))
   }
   return(dist_ylim)
 })
 
 fdistcol_c_select <- reactive({
   dist_col_c_roul=c(((fdist_var_w()+fdist_var_b())/2- fdist_grand_var()), (fdist_var_b()-fdist_grand_var()), (fdist_var_w()-fdist_grand_var()))
   dist_col_c_select = which(abs(dist_col_c_roul)==min(abs(dist_col_c_roul)))
      return(dist_col_c_select)
 })

 
#######Output#########
######################
 
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
  
  
####dist####
  #Within plot 
  output$dist_plot_w <- renderPlot({
    plot(fdist_x_avwithin(), fdist_hx_avwithin(), type="l", xlab="", ylab="",main="",xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent', col=col_w)
  })
  #Between plot
  output$dist_plot_b <- renderPlot({
  plot(fdist_x_bp(), fdist_hx_bp(), type="l", xlab="", ylab="", main="",xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent',col=col_b)
  })
  #Cross-sectional plot
  output$dist_plot_c <- renderPlot({
  plot(fdist_x_grand(), fdist_hx_grand(), type="l", xlab="", ylab="", main="",xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent',col=col_c[fdistcol_c_select()])
  })
  
  
  
}

###############################################################
###############################################################
shinyApp(ui, server)