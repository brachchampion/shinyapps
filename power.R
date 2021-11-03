library(shiny)

ui <- fluidPage(
  # Title
  titlePanel(withMathJax(h1("Power",h3("\\(H_0\\): \\( \\mu=0 \\)  vs  \\(H_a\\): \\( \\mu>0 \\)")))),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Slider for standard deviation
      sliderInput("sd","Std Dev (\\( \\sigma \\))",1, 
                  min = 0, 
                  max = 3, 
                  step = 0.05,
                  animate = animationOptions(interval = 150, loop = FALSE)),
      # Slide for size of test
      sliderInput("size", "Size (\\( \\alpha \\))", 0.05, 
                  min = 0, 
                  max = .5, 
                  step = 0.01,
                  animate = animationOptions(interval = 150, loop = FALSE)),
      # Slider for alternative hypothesis
      sliderInput("mu",
                  "Alternative Hypothesis (\\( \\mu_a \\))",
                  min = 0,
                  max = 5,
                  value = 0,
                  step = .1,
                  animate = animationOptions(interval = 150, loop = FALSE))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw graphs
server <- function(input, output) {
  output$distPlot <- renderPlot({
    mean_a <-input$mu
    set_sd <- input$sd
    alpha <- input$size
    mean_seq <- seq(0,5,.25)
    n_seq <- 1:length(mean_seq)
    power <- rep(0,length(mean_seq))
    for (i in n_seq){
      power[i] <- 1-pnorm(qnorm(1-alpha, sd = set_sd), mean = mean_seq[i], sd = set_sd)
    }
    # graph
    par(mfcol=c(1,2))
    plot.window(xlim = c(-3,8),
                ylim = c(0,dnorm(0,mean = 0, sd = .5)), asp = 10)
    curve(dnorm(x, mean = 0, sd = set_sd),
          from = qnorm(.000001, mean = 0, sd = set_sd), to = qnorm(.999, mean = 0, sd = set_sd),
          xlim = c(-3,8),
          ylim = c(0,dnorm(0,mean = 0, sd = .5)),
          main="Size vs Power", xlab = ""~mu~"", ylab = "")
    curve(dnorm(x, mean = mean_a, sd = set_sd), 
          from =  qnorm(.000001, mean = mean_a, sd = set_sd), to = qnorm(.999, mean = mean_a, sd = set_sd), 
          xlim = c(-3,8),
          main="Size vs Power", xlab = "", ylab = "",
          add = TRUE)
    abline(v = qnorm(1-alpha, mean = 0, sd = set_sd), col = "red")
    text(x=qnorm(1-alpha, mean = 0, sd = set_sd)+2,y=.75,"Reject" ~H[0]~ "=>")
    z<- seq(qnorm(1-alpha, mean = 0, sd = set_sd),qnorm(.9999999, mean = mean_a, sd = set_sd),0.001)
    polygon(x=c(z[1],z,z[length(z)]),
            y=c(0,dnorm(z, mean = mean_a, sd = set_sd),0)
            ,col="lightblue1")
    segments(mean_a,0,mean_a,dnorm(mean_a,mean = mean_a,sd=set_sd),lty=3,col='red')
    text(x = mean_a+.1, y=dnorm(mean_a,mean = mean_a,sd=set_sd)+.025,""~mu[a]~"")
    plot(mean_a,1-pnorm(qnorm(1-alpha,sd = set_sd), mean = mean_a, sd = set_sd),
         xlim = c(0,5), ylim = c(0,1),
         xlab = ""~mu[a]~"",
         ylab = "Power",
         main = "Power to Detect Alternative",
         type="p", pch=19, col = "red")
    lines(mean_seq,power,type="l")
    segments(x0 = mean_a, y0 = 0,
           x1 = mean_a, y1 = 1-pnorm(qnorm(1-alpha, sd = set_sd), mean = mean_a, sd = set_sd),
           lty=3,lwd=2,col="red")
    segments(x0 = 0, y0 = 1-pnorm(qnorm(1-alpha, sd = set_sd), mean = mean_a, sd = set_sd),
             x1 = mean_a, y1 = 1-pnorm(qnorm(1-alpha, sd = set_sd), mean = mean_a, sd = set_sd),
             lty=3,lwd=2,col="red")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

