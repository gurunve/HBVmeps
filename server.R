

server <- function(input, output, session) {
  output$updatetime <- renderText({
    text<- paste(" Last updated:", UpdateInfo,"  ")
    return(text)
    })
  
  
  output$plot1 <- renderImage({
    list(src = c('image/flom1.png'),
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
  output$plot2 <- renderImage({
    list(src = c('image/flom2.png'),
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
  output$plot3 <- renderImage({
    list(src = c('image/sognflom.png'),
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
  
  values <- reactiveValues(fane=1)
  observeEvent(input$Button1,{ values$fane <- 1})
  observeEvent(input$Button2,{ values$fane <- 2})
  observeEvent(input$Button3,{ values$fane <- 3})
  observeEvent(input$Button4,{ values$fane <- 4})
  observeEvent(input$Button5,{ values$fane <- 5})
  observeEvent(input$Button6,{ values$fane <- 6})
  
  output$plot_meps <- renderPlot({
    
    fane=values$fane
    noffelt=25
#    if(fane==6)noffelt=20
#    offset=(fane-1)*25*864
#    i1<-(offset+1)
#    i2<-offset+noffelt*864
#    print(paste("Fane ",fane,i1,i2,sep=" "))
    if (fane ==1) {
      meps1 <- group1
    #  flomtb<-FlomTabellkul[1:25,]
      }
    if (fane ==2) {
      meps1 <- group2
    #  flomtb<-FlomTabellkul[26:50,]
    }
    if (fane ==3) {
      meps1 <- group3
    #  flomtb<-FlomTabellkul[51:75,]
    }
    if (fane ==4) {
      meps1 <- group4
    #  flomtb<-FlomTabellkul[76:100,]
    }
    if (fane ==5) {
      meps1 <- group5
     # flomtb<-FlomTabellkul[101:125,]
    }
    if (fane ==6){
      meps1 <- group6
      noffelt=20
     # flomtb<-FlomTabellkul[126:145,]
    } 
    if(! input$checkbox){
      meps1<-meps1[!(meps1$Member =="kul_50Y"),]
    }
   
    #
#    a<-c()
#    for( i in 1:noffelt){ a<-c(a,meps1$FeltName[i*864])}
#    meps1$facet = factor(meps1$FeltName,levels = a)
    #
    #gp<-ggplot(
    ggplot(meps1,aes(x=DateTime,y=Discharge,colour=Member), drop =FALSE ) + geom_line()+ 
      labs(title=NULL, x="Date Time",y="Discharge")  + 
       facet_wrap("facet" ,ncol = 5, scales = 'free') +
      geom_vline(xintercept=meps1$DateTime[52],colour="green",size=0.2) +
      guides(col=guide_legend(override.aes = list(size=1,alphe=1), nrow=1, title.position = "left")) +
      theme(legend.position ="top",legend.justification = c(0,3)) +
      scale_colour_manual(
        values = c("det"="black","en0"="orangered","en1"="orange","en2"="orange2",
                   "en3"="green","en4"="blue","en5"="slateblue1","en6"="magenta",
                   "en7"="sienna1","en8"="purple","en9"="turquoise1","Obs"="black",
                  "kul_Mean"="Yellow","kul_5Y"="Orange","kul_50Y"="Red")) +
      scale_linetype_manual( 
        values = c("det"="dot","en0"="solid","en1"="solid","en2"="solid",
                   "en3"="solid","en4"="solid","en5"="solid","en6"="solid",
                   "en7"="solid","en8"="solid","en9"="solid","Obs"="solid" ,
                   "kul_Mean"="solid","kul_5Y"="solid","kul_50Y"="solid")) 
     
   
    
  }, height= 1000)
}


