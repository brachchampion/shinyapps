library(shiny)

ui <- fluidPage(
  # Title
  titlePanel("Confidence Interval Simulation"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Slider for sample size
      sliderInput("sample_size","Sample Size",1000, 
                  min = 100, 
                  max = 10000, 
                  step = 10),
      # Slide for number of samples
      sliderInput("num_samples", "Number of Samples", 50, 
                  min = 10, 
                  max = 100, 
                  step = 1),
      # Slider for confidence level
      sliderInput("ci_level","Confidence Level", 0.95,
                  min = 0.01,
                  max = 0.99,
                  step = 0.01),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot") #, width = "100%", height = "800px")
    )
  )
)
# Define server logic required to draw graphs
server <- function(input, output) {
  output$distPlot <- renderPlot({
  set.seed(123) # consistent results
  # Set the parameters of the simulation
  sample_size <- input$sample_size
  num_samples <- input$num_samples
  ci_level    <- input$ci_level
  # theoretical pop mean / standard deviation (sd)
  pop_mean    <- 0
  pop_sd      <- 1
  # drawing randomly from the distribution and forming it into a matrix
  r_draws     <- matrix(rnorm(sample_size*num_samples, pop_mean, pop_sd),num_samples,sample_size)
  # Constructing the sample mean as the row mean
  sample_mean <- apply(r_draws,1,mean)
  # finding the margin of error
  moe         <- qnorm(1-(1-ci_level)/2)*pop_sd/sqrt(sample_size)
  # binding x\bar-moe and x\bar+moe as two columns
  CI          <- cbind(sample_mean-moe,sample_mean+moe)
  # the proportion of CI that have population mean within the CI
  mean( (pop_mean>CI[,1]) & (pop_mean<CI[,2]))

  require(ggplot2)

  # a logical vector, TRUE if the confidence intercal captures the population mean
  is_mu_in_CI <- ((pop_mean>CI[,1]) & (pop_mean<CI[,2]))

  ## making the plot
  ggplot()+
    geom_linerange(
      aes(x=seq(1,num_samples),
          ymin = CI[,1], 
          ymax = CI[,2], 
          col=is_mu_in_CI
      )
    )+ 
    scale_color_manual(values=c("TRUE"="blue","FALSE"="red"))+
    guides(col=FALSE)+
    labs(title="Confidence Intervals of Sample Mean From Std. Normal Distribution", x="Sample Number",y="")+
    geom_hline(yintercept=pop_mean, col="black",lty=2)+
    theme_classic()+
    annotate(geom="text", x=num_samples/2, y=min(CI[,1])-abs(min(CI[,1]))/4, 
             label=paste(toString(round(mean(is_mu_in_CI),4)*100),
                         "% of confidence intervals contain the population mean",sep=""),
             color="black")+
    coord_cartesian(ylim = c(min(CI[,1])-abs(min(CI[,1]))/2,max(CI[,2])+abs(max(CI[,2]))/2))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)