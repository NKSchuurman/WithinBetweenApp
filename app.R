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
                        
                        radioButtons(inputId="dist_ICCornot", label=NULL, choices=list("Variance", "ICC"),inline=TRUE), 
                        
                       
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
    ),         
    
    tabPanel("Dynamic (VAR) Networks",
   
   fluidRow(
     
     column(2,
            wellPanel(
              radioButtons(inputId="netcortype", label="Type of correlations", choices=list("Pairwise", "Partial"),inline=TRUE), 
              
              radioButtons(inputId="net_nvars", label="Number of nodes", choices=list(3, 5),inline=TRUE), 
              
              
              conditionalPanel(
                condition = "input.net_nvars == 3",
                # Input: ICCs
                radioButtons(inputId="net3_lagged", label="Lagged effects matrix", choices=list("a", "b", "c"),inline=TRUE),
                radioButtons(inputId="net3_res", label="Contemporaneous residual covariance matrix", choices=list("a", "b", "c"),inline=TRUE),
                radioButtons(inputId="net3_between", label="Between person correlation matrix", choices=list("a", "b", "c"),inline=TRUE),
                sliderInput(inputId="net3_ICC1", label="ICC1", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net3_ICC2", label="ICC2", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net3_ICC3", label="ICC3", min=0, max=1, value=0.5,step=.1)
              ),
              
              conditionalPanel(
                condition = "input.net_nvars == 5",
                # Input: ICCs
                radioButtons(inputId="net5_lagged", label="Lagged effects matrix", choices=list("a", "b","c"),inline=TRUE),
                radioButtons(inputId="net5_res", label="Contemporaneous residual covariance matrix", choices=list("a", "b","c"),inline=TRUE),
                radioButtons(inputId="net5_between", label="Between person correlation matrix", choices=list("a", "b", "c"),inline=TRUE),
                sliderInput(inputId="net5_ICC1", label="ICC1", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net5_ICC2", label="ICC2", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net5_ICC3", label="ICC3", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net5_ICC4", label="ICC4", min=0, max=1, value=0.5,step=.1),
                sliderInput(inputId="net5_ICC5", label="ICC5", min=0, max=1, value=0.5,step=.1)
              )
              
              
            )       
     ),
     
      #  for displaying outputs 
     column(10,
            fluidRow(
              column(6, 
                     plotOutput("netstdreg_wp"),
                     plotOutput("netpw_bp")
              ),
            
              column(6,
                    plotOutput("netpw_wp"),
                    plotOutput("netpw_c"))
            )
     
     )   
   )
  )
 )
)

################################################################
################################################################
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
###general user input independent operations###
#############################################  
  library(MASS)
  col_b = "#FF1545"
  col_w = "#00CFFF"
  col_c = c("#9807FF", "#9F01B3", "#4C18EA")
  
###dist###
  dist_grand_mean= 0
  dist_bp_mean=dist_grand_mean
  dist_av_wp_mean=dist_grand_mean
  
  ####netw####
  library(qgraph)
  
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
 
 ###netw####

 
 fnetne <- reactive({
   
   if(input$net_nvars == 3){
    ne=3
     return(ne)
   }
   if(input$net_nvars == 5){
  ne=5
     return(ne)
   }
   
 })
 
 fnetny <- reactive({
   ny=fnetne()
   return(ny)
 })
 
 fnetH <- reactive({
   
   if(input$net_nvars == 5)
     { 
     if(input$net5_lagged == 'c'){
       H=matrix(c(0.5,0.25,0.25,0.25,0.0,
                  0.0,0.5,0.0,0.0,0.0,
                  0.0,0.0,0.4,0.0,-.2,
                  0,0.0,0.0,0.5,0.0,
                  -.2,0.0,0.0,0.0,0.4),fnetne(),fnetne(),byrow=TRUE)
     return(H)
     }
     if(input$net5_lagged == 'a'){
       H=matrix(c(0.7,0,0,0,0,
                  0,0.5,0,0,0,
                  0,0,0.5,0,0,
                  .3,.3,.3,0.5,.3,
                  0,0,0,0,0.6),fnetne(),fnetne(),byrow=TRUE)
       return(H)
     }
     if(input$net5_lagged == 'b'){
       H=matrix(c(0.7,-.3,-.15,0,-.1,
                   -.1,0.5,0,-.05,-.15,
                   0,-.2,0.5,0,0,
                   0,0,0,0.5,-.1,
                   0,0,0,-.2,0.6),fnetne(),fnetne(),byrow=TRUE)
       return(H)
     }
   }
 
 if(input$net_nvars == 3)
 { 
   if(input$net3_lagged == 'c'){
     H=array(NA,c(fnetne(),fnetne()))
     H[1,1]= .5 
     H[1,2]= -.2 
     H[1,3]= 0
     H[2,1]= -.1
     H[2,2]= .3 
     H[2,3]= 0.1
     H[3,1]= -.1
     H[3,2]= 0.06
     H[3,3]= 0.4
     return(H)
   }
   if(input$net3_lagged == 'a'){
     H=array(NA,c(fnetne(),fnetne()))
     H[1,1]= .35
     H[1,2]= .1
     H[1,3]= .05
     H[2,1]= .3
     H[2,2]= .6 
     H[2,3]= 0.1
     H[3,1]= .2
     H[3,2]= .2
     H[3,3]= 0.5
     return(H)
   }
   if(input$net3_lagged == 'b'){
     H=array(NA,c(fnetne(),fnetne()))
     H[1,1]= .4
     H[1,2]= -.05
     H[1,3]= -.2
     H[2,1]= -.3
     H[2,2]= .5
     H[2,3]= -.05
     H[3,1]= -.1
     H[3,2]= -.2
     H[3,3]= 0.55
     return(H)
   }
 }
})
 
 fnetResCov <- reactive({
   if(input$net_nvars == 5 & input$net5_res == 'a' ){
     ResCov=matrix(c(0.5,0.0,0.0,0.0,0.0,
                     0.0,0.5,0.0,0.0,0.0,
                     0.0,0.0,0.5,0.0,0.0,
                     0.0,0.0,0.0,0.5,0.0,
                     0.0,0.0,0.0,0.0,0.5),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   if(input$net_nvars == 5 & input$net5_res == 'b'){
     ResCov=matrix(c(0.5,0.1,0.1,0.1,0.1,
                     0.1,0.5,0.1,0.1,0.1,
                     0.1,0.1,0.5,0.1,0.1,
                     0.1,0.1,0.1,0.5,0.1,
                     0.1,0.1,0.1,0.1,0.5),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   if(input$net_nvars == 5 & input$net5_res == 'c'){
     ResCov=matrix(c(0.5,-0.1,-0.1,-0.1,-0.1,
                     -0.1,0.5,-0.1,-0.1,-0.1,
                     -0.1,-0.1,0.5,-0.1,-0.1,
                     -0.1,-0.1,-0.1,0.5,-0.1,
                     -0.1,-0.1,-0.1,-0.1,0.5),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   if(input$net_nvars == 3 & input$net3_res == 'a' ){
     ResCov=matrix(c(
       .5,0,0,
       0,.5,0,
       0,0,.5
       ),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   if(input$net_nvars == 3 & input$net3_res == 'b'){
     ResCov=matrix(c(
       1,.1,.1,
       .1,1,.1,
       .1,.1,1
     ),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   if(input$net_nvars == 3 & input$net3_res == 'c'){
     ResCov=matrix(c(
       1,-.1,-.1,
       -.1,1,-.1,
       -.1,-.1,1
     ),fnetne(),fnetne(),byrow=T)
     return(ResCov)
   }
   
 })
 

 fnetbcor <- reactive({
   if(input$net_nvars == 5 & input$net5_between == 'a' ){
     cormat_bp= matrix(c(
       1,.15, 0.1,0,0,
       .15,1,0.1,0,.3,
       .1,.1,1,0,0,
       0,0,0,1,.5,
       0,.3,0,.5,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
   if(input$net_nvars == 5 & input$net5_between == 'b'){
     cormat_bp= matrix(c(
       1,-.5, -.3,0,0,
       -.5,1,-.1,-.2,0,
       -.3,-.1,1,-.1,0,
       0,-.2,-.1,1,0,
       0,0,0,0,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
   if(input$net_nvars == 5 & input$net5_between == 'c'){
     cormat_bp= matrix(c(
       1,-.2,.2,0,.3,
       -.2,1,0,0,-.1,
       .2,0,1,-.1,.2,
       0,0,-.1,1,-.15,
       .3,-.1,.2,-.15,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
   if(input$net_nvars == 3 & input$net3_between == 'b' ){
     cormat_bp= matrix(c(
       1,.2, 0.1,
       .2,1,0.1,
       .1,.1,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
   if(input$net_nvars == 3 & input$net3_between == 'c'){
     cormat_bp= matrix(c(
       1,-.2, -.1,
       -.2,1,-.1,
       -.1,-.1,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
   if(input$net_nvars == 3 & input$net3_between == 'a'){
     cormat_bp= matrix(c(
       1,-.3, 0,
       -.3,1,.1,
       0,.1,1
     ),fnetny(),fnetny(),byrow=TRUE)
     return(cormat_bp)
   }
 })
   
 
fnet_covmat_wp <- reactive({
  Id =diag(fnetne()*fnetne())
 covmat_wp=array(NA,c(fnetne(),fnetne()))
 cvvec_wp= solve(Id-( fnetH()%x% fnetH())) %*% c(fnetResCov()) ###calculate the covmatrix
 cvmat_wp= matrix(cvvec_wp,fnetne(),fnetne())
 covmat_wp= cvmat_wp ##for saving
 return(covmat_wp)
})

fnet_cormat_wp <- reactive({
  cormat_wp=array(NA,c(fnetne(),fnetne()))
  cormat_wp= cov2cor(fnet_covmat_wp()) ##for saving
  return(cormat_wp)
})

fnet_covmat_bp <- reactive({
  if(input$net_nvars == 5){
  covmat_wp <- fnet_covmat_wp()
  bp_var1 = covmat_wp[1,1]/ (1-input$net5_ICC1) - covmat_wp[1,1]
  bp_var2 = covmat_wp[2,2]/ (1-input$net5_ICC2) - covmat_wp[2,2]
  bp_var3 = covmat_wp[3,3]/ (1-input$net5_ICC3) - covmat_wp[3,3]
  bp_var4 = covmat_wp[4,4]/ (1-input$net5_ICC4) - covmat_wp[4,4]
  bp_var5 = covmat_wp[5,5]/ (1-input$net5_ICC5) - covmat_wp[5,5]
  covmat_bp = fnetbcor()
  covmat_bp[,1] = covmat_bp[,1]*(sqrt(bp_var1))
  covmat_bp[,2] = covmat_bp[,2]*(sqrt(bp_var2))
  covmat_bp[,3] = covmat_bp[,3]*(sqrt(bp_var3))
  covmat_bp[,4] = covmat_bp[,4]*(sqrt(bp_var4))
  covmat_bp[,5] = covmat_bp[,5]*(sqrt(bp_var5))
  covmat_bp[1,] = covmat_bp[1,]*(sqrt(bp_var1))
  covmat_bp[2,] = covmat_bp[2,]*(sqrt(bp_var2))
  covmat_bp[3,] = covmat_bp[3,]*(sqrt(bp_var3))
  covmat_bp[4,] = covmat_bp[4,]*(sqrt(bp_var4))
  covmat_bp[5,] = covmat_bp[5,]*(sqrt(bp_var5))
  return(covmat_bp)
  }
  if(input$net_nvars == 3){
    covmat_wp <- fnet_covmat_wp()
    bp_var1 = covmat_wp[1,1]/ (1-input$net3_ICC1) - covmat_wp[1,1]
    bp_var2 = covmat_wp[2,2]/ (1-input$net3_ICC2) - covmat_wp[2,2]
    bp_var3 = covmat_wp[3,3]/ (1-input$net3_ICC3) - covmat_wp[3,3]
    covmat_bp = fnetbcor()
    covmat_bp[,1] = covmat_bp[,1]*(sqrt(bp_var1))
    covmat_bp[,2] = covmat_bp[,2]*(sqrt(bp_var2))
    covmat_bp[,3] = covmat_bp[,3]*(sqrt(bp_var3))
    covmat_bp[1,] = covmat_bp[1,]*(sqrt(bp_var1))
    covmat_bp[2,] = covmat_bp[2,]*(sqrt(bp_var2))
    covmat_bp[3,] = covmat_bp[3,]*(sqrt(bp_var3))
    return(covmat_bp)
  }
 })

fnet_covmat_cross <- reactive({
covmat_cross= fnet_covmat_bp() + fnet_covmat_wp()
return(covmat_cross)
})


fnet_cormat_cross <- reactive({
  cormat_cross= cov2cor(fnet_covmat_cross())
  return(cormat_cross)
})

fnet_stH <- reactive({
st_H<-fnetH()
covmat_wp=fnet_covmat_wp()
for (i in 1:fnetne()){
st_H[i,]<-st_H[i,]/sqrt(covmat_wp[i,i])
st_H[,i]<-st_H[,i]*sqrt(covmat_wp[i,i])
}
return(st_H)
})

###partial correlations results##

#precision matrices
##WP
fnet_partial_wp <- reactive({
premat_wp <- solve(fnet_covmat_wp())
partial_wp <- -cov2cor(premat_wp)
diag(partial_wp) <- 1
return(partial_wp)
})
##BP
fnet_partial_bp <- reactive({
premat_bp <- solve(fnet_covmat_bp())
partial_bp <- -cov2cor(premat_bp)
diag(partial_bp) <- 1
return(partial_bp)
})
##Cross
fnet_partial_cross <- reactive({
premat_cross <- solve(fnet_covmat_cross())
partial_cross <- -cov2cor(premat_cross)
diag(partial_cross) <- 1
return(partial_cross)
})

fnet_plot_w <- reactive({
  if(input$netcortype == "Pairwise"){  
  return(fnet_cormat_wp())
  }
  if(input$netcortype == "Partial"){  
    return(fnet_partial_wp())
  }
})

fnet_plot_b <- reactive({
  if(input$netcortype == "Pairwise"){  
    return(fnetbcor())
  }
  if(input$netcortype == "Partial"){  
    return(fnet_partial_bp())
  }
})
  
fnet_plot_cross <- reactive({
  if(input$netcortype == "Pairwise"){  
    return(fnet_cormat_cross())
  }
  if(input$netcortype == "Partial"){  
    return(fnet_partial_cross())
  }
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
    plot(fdist_x_avwithin(), fdist_hx_avwithin(), type="l", xlab="", ylab="",xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent', col=col_w, main=paste("Within Variance =", fdist_var_w()))
  })
  #Between plot
  output$dist_plot_b <- renderPlot({
  plot(fdist_x_bp(), fdist_hx_bp(), type="l", xlab="", ylab="",xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent',col=col_b, main=paste("Between Variance =", fdist_var_b()))
  })
  #Cross-sectional plot
  output$dist_plot_c <- renderPlot({
  plot(fdist_x_grand(), fdist_hx_grand(), type="l", xlab="", ylab="", xlim= fdist_xlim(),ylim= fdist_ylim(), lwd=6, bg='transparent',col=col_c[fdistcol_c_select()], main=paste("Cross-sectional Variance =", fdist_grand_var()))
  })
  
 
####netw#####  
  
 output$netstdreg_wp <- renderPlot({ qgraph(fnet_stH(),maximum=.9,fade=FALSE,layout="circle",vsize=12,edge.width=3.5,title="Within-Person Lagged Network \n Standardized Regression Coefficients",label.prop=1.2,posCol=c("red"),negCol=c("blue"),edge.labels=FALSE,edge.label.cex=3, negDashed=TRUE, mar=c(3,4,3,4)) 
   })
  output$netpw_wp<-  renderPlot({qgraph(fnet_plot_w(),maximum=.9,fade=FALSE,layout="circle",vsize=12,edge.width=3.5,title="Within-Person \n Contemporaneous\n Correlations",label.prop=1.2,posCol=c("red"),negCol=c("blue"),edge.labels=FALSE,edge.label.cex=3, negDashed=TRUE, mar=c(3,4,3,4)) 
    })
  output$netpw_bp<-  renderPlot({qgraph(fnet_plot_b(),maximum=.9,fade=FALSE,layout="circle",vsize=12,edge.width=3.5,title="Between-Person \n Correlations",label.prop=1.2,posCol=c("red"),negCol=c("blue"),edge.labels=FALSE,edge.label.cex=3, negDashed=TRUE, mar=c(3,4,3,4)) 
    })
  output$netpw_c<-  renderPlot({qgraph(fnet_plot_cross(),maximum=.9,fade=FALSE,layout="circle",vsize=12,edge.width=3.5,title="Cross-sectional \n Correlations",label.prop=1.2,posCol=c("red"),negCol=c("blue"),edge.labels=FALSE,edge.label.cex=3, negDashed=TRUE, mar=c(3,4,3,4))
    })
  

  
}

###############################################################
###############################################################
shinyApp(ui, server)