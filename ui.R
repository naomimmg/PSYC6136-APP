ui <- fluidPage(
  titlePanel(
    windowTitle = "Categorical Data Analysis",
    tagList(
      div(style = "display: flex; align-items: center; padding-top: 10px; margin-bottom: 20px;",
          img(src = "logo.png", height = "100px", style = "margin-right: 20px;"),
          div(
            h1("Categorical Data Analysis", 
               style = "font-weight: bold; margin: 0; display: inline-block;")
          )
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #f8f9fa;",
      h3("How to Use This App"),
      tags$b("Step 1. Choose Data"), p(""),
      tags$b("Step 2. Clean Data"), p("Verify factor levels and order."), 
      tags$b("Step 3. Specify Model"), p("Choose fit type (e.g., JOINT, MUTUAL)."), 
      tags$b("Step 4. Plot"), p("Customize and view your Mosaic plot.") 
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        # Panel 1: Data Selection - [cite: 45]
        tabPanel("1. Data Selection", br(),
                 wellPanel(
                   selectInput("select_sample_dat", "Choose your Data! \U0001f642", names(data_sets))
                 ),
                 uiOutput("table_preview")),
        
        tabPanel("2. Data Cleaning", br(),
                 h4("Factor Levels & Variable Order"),
                 verbatimTextOutput("level_preview")), # [cite: 23]
        
        tabPanel("3. Model Specification", br(),
                 wellPanel(
                   selectInput("fit_type", "Fit Type (Sequential Models):", 
                               choices = c("JOINT [AB][C]" = "joint", 
                                           "MUTUAL [A][B][C]" = "mutual",
                                           "CONDIT [AC][BC]" = "condit",
                                           "MARKOV (Chain)" = "markov")),
                   
                   selectInput("res_type", "Residual Type:", 
                               choices = c("Pearson (GF)" = "pearson", 
                                           "Likelihood Ratio (LR)" = "deviance",
                                           "Freeman-Tukey (FT)" = "ft")),
                   
                   hr(),
                   helpText("Note: Formulae like ~ A * B + C are used for JOINT independence models.") 
                 )
        ), 
        
        tabPanel("4. Mosaic Plot \U0001f44f", br(),
                 column(4, 
                        wellPanel(
                          selectInput("fill_type", "Fill Type:", 
                                      choices = c("M45", "LR", "M0", "GRAY", "HLS")),
                          
                          # Text height as percentage - 
                          sliderInput("text_height", "Text Height %:", 
                                      min = 50, max = 150, value = 100),
                          
                          # Split directions from notes: H and V - 
                          checkboxGroupInput("split", "Split Directions:", 
                                             choices = c("Horizontal" = "H", "Vertical" = "V"), 
                                             selected = c("H", "V"))
                        )
                 ),
                 column(8, 
                        plotOutput("mosaic_plot"))
        )
      )
    )
  )
)
