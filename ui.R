library(shiny)

# Define UI for Linear Regression demo 
shinyUI(pageWithSidebar(
  
  #  Demo title
  headerPanel("Linear Regression"),
  
  sidebarPanel(
    
    tags$h3("Data Generation"),
    
    sliderInput("nobs", "Number of Observations:", 
                min=10, max=500, value=100),
    br(),
    selectInput("dgp", "Data Generating Process:",
                list("y=4+2*x+u", 
                     "y=4+2*x-.5*x2+u",
                     "y=-10-1.5*x+x2+u",
                     "y=-3*x-.6*x2+u",
                     "y=5+2*x-.1*expx+u",
                     "y=.5*expx+u*10",
                     "y=4-2*x2+.4*expx-x*2+u*5",
                     "y=x+x2+x3+x5+u"
                     
                ),
                selected = "y=4+2*x-.5*x2+u"),
    
    br(),
    sliderInput("rseed", "Random Seed:", 
                min=1, max=100, value=27),
    
    
    
    
    sliderInput("sdx", "Standard deviation of X:", 
                min=.25, max=5, value=2.5, step=.25),
    
    sliderInput("sdu", "Standard deviation of u:", 
                min=1, max=50, value=2, step=1),
    
    
    br(),
    h3("Training Data"),
    sliderInput("train", "Proportion of the data for the training [ % ]:", 
                min=1, max=100, value=80),
    
    br(),
    h3("Prediction"),
    
    selectInput("regression", "Select Learning Model:",
                list("y~x", 
                     "y~x2",
                     "y~expx",
                     "y~x+x2",
                     "y~x+expx",
                     "y~x+x2+expx",
                     "y~x+x2+x3+x4+x5"
                ),
                selected = "y~x+x2"),
    
    checkboxInput("constant", "Include Intercept", TRUE),
    
    
    h5("Plot"),
    checkboxInput("showdata", "Show Test Data", TRUE),
    
    checkboxInput("predict", "Show Regression Curve", TRUE),
    checkboxInput("resid", "Show Residuals", TRUE),
    
    
    
    br(),
    
    p('Source code available at',
      a('GitHub', href="https://github.com/FBracun/DevelopDataProducts.git", target="_blank")),
    
    
    p('Presentation available at',
      a('RPubs', href="http://rpubs.com/FrancB/23324/", target="_blank"))
    
   
    ),
  
  
  
  # Show the main display
  mainPanel(
    
    tabsetPanel(
          
      tabPanel('Regression Model',
               plotOutput("scatter"),
               br(),
               h4("Statistics of the trained model:", style = "color:blue"),
               tableOutput("lmStats"),
               h4("Coefficients of the trained model:", style = "color:blue"),
               tableOutput("lmResults")
      ),
      
      tabPanel('Prediction Quality',
               h3("Plot of residual error as a function of prediction", style = "color:orange"),
               plotOutput("residuals"),
               h4(textOutput("R_sq_test"), style = "color:orange"),
               h4(textOutput("RMSE_test"), style = "color:orange"),
               h4(textOutput("R_sq_train"), style = "color:blue"),
               h4(textOutput("RMSE_train"), style = "color:blue")
           
      ),
      
      tabPanel('Training Data',
               h5(textOutput("datacaption")),
               #uiOutput("datacaption"),
               dataTableOutput('train')
               
      ),
      
      tabPanel('Test Data',
              h5(textOutput("datacaptionTest")),
             #uiOutput("datacaption"),
              dataTableOutput('test')
               
      ),
      
      ###################################################
      
      tabPanel('Documentation',
               

               h3(textOutput("Document"), style = "color:gray"),



               p(style="text-align:justify",
                'This R Shiny web app allows the user to perform linear regression on 
                a simulated dataset. 
                An app allows for instant interactivity returning:'),              
              
               p(HTML('<ul>'),
                 HTML('<li>'),"Linear Regression Model coefficient estimates and statistics,",HTML('</li>'),
                 HTML('<li>'),"Prediction Graph,",HTML('</li>'),
                 HTML('<li>'),"Plot of residual error as a function of prediction together with R-squared and RMSE on testing and training data,",HTML('</li>'),
                 HTML('<li>'),"Training and Test Data,",HTML('</li>'),
                 HTML('</ul>')),
               
               p(style="text-align:justify",'through easy to use user interface controls.'),
               
               br(),
               
               h4("User Interface Controls"),
               
               p(style="text-align:justify",
                 'The app allows user to control input parameters by selecting  various values through the widgets 
                  on the left-hand side panel.
                  There are 4 groups of input parameters the user can control:'), 
               
               p(HTML('<ol>'),
                 HTML('<li>'),"Data Generation",HTML('</li>'),
                 
                 HTML('<ul>'),
                 HTML('<li>'),
                 "The user has control of how many observations to generate. The minimum, maximum
                 and initial value are ( min=10, max=500, value=100 ).
                 
                 ", HTML('</li>'),
                 HTML('<li>'),"
                 The user also has control of how to generate the dependent variable y.
                 From a box with choices to select from user can select equation for Data Generating Process.
                 (See the table below for explanations on the notation of equations.)  
                 ",HTML('</li>'),
                 HTML('<li>'),"
                 The app only has a single x variable which is randomly drawn from a normal distribution with mean 2 
                 and standard deviation specified by the user. There is also an error term u which has mean 0 and 
                 standard deviation specified by the user. By moving three sliders the user can control seed, 
                 standard deviation of input variable x, and standard deviation of an error term u. By setting the seed 
                 to the pseudorandom number generator, we make our work reproducible: someone redoing it will see the exact same results.
                 ",HTML('</li>'),
                 HTML('</ul>'),
                 

                 HTML('<li>'),"Training Data",HTML('</li>'),
                 HTML('<ul>'),
                 HTML('<li>'),
                 "User can further split data (observations) into training and testing data. The default value is 80% of training data. 
                 ",HTML('</li>'),
                 HTML('</ul>'),

                  HTML('<li>'),"Prediction",HTML('</li>'),
                  HTML('<ul>'),
                  HTML('<li>'),
                  "
                  The user  has control of how to generate the dependent variable y.
                 From a box with choices to select from user can select a model for learning process.
                 (See the table below for explanations on the notation of equations.)
                  ",HTML('</li>'),
                 
                  HTML('<li>'),
                 "The user can also choose whether to include the intercept in the model.                                   
                  ",HTML('</li>'),           
                 
                 HTML('</ul>'),
                 
                                                 
                 
                 HTML('<li>'),"Plot",HTML('</li>'),
                 HTML('<ul>'),
                 HTML('<li>'),
                 "In the end, the user can choose three options to display items on the graph in 
                  the Regression Graph tab, i.e. whether the graph displays test data, 
                  regression curve, and residuals.                                   
                  ",HTML('</li>'),
                 
                 
                 
                 HTML('</ul>'),
                 HTML('</ol>')),
               
               
                              
               br(),
               
               h4("Output options"),
               
               p(style="text-align:justify",
                 'There are four tabbed windows in the output region where users can select the type of output they want:'), 
               
               p(HTML('<ol>'),
                 HTML('<li>'),"Regression Model",HTML('</li>'),
                 
                 HTML('<ul>'),
                 HTML('<li>'),
                 "Linear regression model graph depending on user choices shows regression curve, test data and residuals. 
                 ", HTML('</li>'),

                 HTML('<li>'),
                 "
                 Below the graph are two tables with model coefficient estimates and statistics.

               
                 ", HTML('</li>'),
                 
                 HTML('</ul>'),
                 
                 HTML('<li>'),"Prediction Quality",HTML('</li>'),
                 
                 HTML('<ul>'),
                 HTML('<li>'),
                 "
                  This tab shows plot of residual error as a function of prediction, 
                  and R-squared and RMSE on test and training data.

                 ", HTML('</li>'),
                 
                 HTML('</ul>'),
                 
                 
                 HTML('<li>'),"Training Data",HTML('</li>'),
                 HTML('<ul>'),
                 HTML('<li>'),
                 "
                  This tab contains a table of training data.


                  ",HTML('</li>'),
                 
                          
                 
                 HTML('</ul>'),
                 
                 
                 
                 HTML('<li>'),"Test Data",HTML('</li>'),
                 HTML('<ul>'),
                 HTML('<li>'),
                 "

                  This tab contains a table of test data.

                  ",HTML('</li>'),

                 HTML('</ul>'),
                 HTML('</ol>')),
               
               
               br(),
               
               
               
               h4("Equations for Data Generation and Regression Model"),
               br(),
               
               #h5('Data Generation'),
               uiOutput('dataGenerationexpr'),
               
               #br(),
               
               #h5('Regression Model'),
               #uiOutput('learningModelexpr'),
               
               
               
               br(),
               
             
               strong('Author'),
               p('Franc Bracun'
                
               br(),
               
               strong('Code'), 
               br(),
              
               p('Source code available at',
                 a('GitHub', href="https://github.com/FBracun/DevelopDataProducts.git", target="_blank")),
               br(),
               strong('Presentation'), 
               br(),
               p('Presentation available at',
                 a('RPubs', href="http://rpubs.com/FrancB/23324/", target="_blank"))
               
               
               )
       )
     )
   ) 
)
