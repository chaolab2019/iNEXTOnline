library(ggplot2)
library(igraph)
library(shiny)


shinyUI(navbarPage(
  id="index",
  title="iNEXT Online (Sept. 2016)",selected = "iNEXT",
  
  tabPanel("Sample Completeness Curve",h2("Sample Completeness Curve"),value="sc"),
  tabPanel("Interpolation/Extrapolation",h2("Interpolation/Extrapolation (iNEXT)"),value="iNEXT"),
  tabPanel("Asymptotic Analysis",h2("Asymptotic Analysis"),value="Profile"),
  fluidPage(
    tags$head(tags$link(rel = "icon", type = "image/x-icon", href = 
                          "http://s6d6.turboimg.net/sp/6a9a0b9be1434059090279b889372b93/iNEXT_High_quality.png"),
              tags$style(HTML(" .shiny-output-error-validation {
                              color: red;}
                              "))
              ),
    fluidRow(
      column(3,
             tags$em("//",style="color:white;"),
             tags$img(src="http://s6d6.turboimg.net/sp/6a9a0b9be1434059090279b889372b93/iNEXT_High_quality.png" ,alt="W3Schools.com",width = "65px", height = "65px"),
             # tags$figure(src = "http://s6d4.turboimg.net/sp/0a1023688902103219468ecfb3cbe3e5/_.png", width = "65px", height = "65px"),
             tags$em("/////",style="color:white;"),
             
             actionButton("goButton",span("Run!",style="font-size: 30px"),icon("rocket","fa-3x")),
             
             h4("Data Setting"),
             wellPanel(
                  radioButtons("source","Choose one:",choices=c("Demo data"="demo","Upload data"="upload","Key in data"="keyin")),
                  radioButtons("datatype","Select data type:",choices=c("abundance data"="abundance","incidence data"="incidence_freq")),
                  conditionalPanel("input.source=='upload'",
                                   fileInput("files","Choose File (txt)", multiple = TRUE,accept = c('text/tab-separated-values'))
                                   ),
                  conditionalPanel("input.source=='demo' & input.datatype=='abundance'",
                                   radioButtons("twodata","Choose demo data:",choices=c("Spiders"="spider","Beetles"="beetles"))
                                   ),
                  ##############
                  conditionalPanel("input.source=='keyin' & input.datatype=='abundance'",tags$textarea(id="copyAndPaste_abun", rows = 8, cols=40,
                                "Girdled 46 22 17 15 15  9  8  6  6  4  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1 \nLogged 88 22 16 15 13 10  8  8  7  7  7  5  4  4  4  3  3  3  3  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1")),
                  conditionalPanel("input.source=='keyin' & input.datatype=='incidence_freq'",tags$textarea(id="copyAndPaste_inc", rows = 8, cols=63,
                                 "h1070m 150  99  96  80  74  68  60  54  46  45  45  43  43  39  38  36  34  32  31  31  31  30  26  25  25  25  24  23  22  21  19  19  18  16  16  16  16  15  14  13  13  13  13  12 12  12  11  11  11  11  10   9   8   7   7   7   7   7   7   6   6   6   5   4   4   4   3   3   3   3   3   3   3   3   3   3   3   3   3   2   2   2   2   2   2   2   2   2 2   2   2   2   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 \nh1500m 200 144 113  79  76  74  73  53  50  43  33  32  30  29  25  25  25  24  23  23  19  18  17  17  11  11   9   9   9   9   6   6   5   5   5   5   4   4   3 3   2   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1   1 \nh2000m 200  80  59  34  23  19  15  13   8   8   4   3   2   2   1")),
                  conditionalPanel("input.source=='keyin' & input.datatype=='abundance'",p(em("(Input : Community label follows by species abundances; Press enter key to type in data of the next community.)"))),
                  conditionalPanel("input.source=='keyin' & input.datatype=='incidence_freq'",p(em("(Input : Community label follows by species incidence frequency; Press enter key to key in data of the next community.)"))),
                  uiOutput("choose_dataset"),
                  #                    conditionalPanel("input.source=='demo'", uiOutput("choose_datasetdemo")),
                  #                    conditionalPanel("input.source=='upload'", uiOutput("choose_dataset")),
                  p(em("Using ctrl / command key to select multiple datasets you want"))
             ),

             h4("General Setting"),
             wellPanel(
               
               conditionalPanel("input.index=='iNEXT'",
                                radioButtons("q","Diversity order q:",choices=c("q=0"=0,"q=1"=1, "q=2"=2 )),
                                radioButtons("cutpoint","Choose one:",choices=c("Specify endpoint and # of knots"="knots","Specify sample sizes"="sample")),
                                conditionalPanel("input.cutpoint=='knots'",
                                                 uiOutput("choose_size"),
                                                 numericInput("knots", "Number of knots",  min = 1,  max = 2000, value = 40)
                                ),
                                conditionalPanel("input.cutpoint=='sample'",
                                                 h4("Sample size"),
                                                 tags$textarea(id="size", rows = 3, cols=28,"")
                                )
                                ),
               numericInput("nboot", "Number of bootstraps (Time consuming, enter '0' to skip bootstrap)",  min = 1,  max = 200, value = 50),
               numericInput("conf", "Level of confidence interval",  min = 0,  max = 1, value = 0.95),
               conditionalPanel("input.index=='sc'",
                                h4("Type in order q"),
                                tags$textarea(id="orderq", rows = 3, cols=40,"0.00 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00")
                                ),
               conditionalPanel("input.index=='Profile'",
                                h4("Type in diversity orders"),
                                tags$textarea(id="orderq", rows = 3, cols=40,"0.00 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00")
                                )
               )
             

      ),
      column(9,
             conditionalPanel("input.index=='sc'",
                              tabsetPanel(

                                # tabPanel("Sample Completeness Curve",icon=icon("picture-o"),
                                #          h4("Estimated Sample Completeness Curve"),
                                #          plotOutput("plotprofile"),
                                #          downloadButton('downloadplotprofile','Save as PNG File'),
                                #          h4("Estimated Diversity and Entropy for Each Order"),
                                #          tableOutput("Dprofile_table_diversity"),
                                #          tableOutput("Dprofile_table_entropy"),
                                #          downloadButton('downloadprofile2','Save as CSV File'),
                                # 
                                #          br(),
                                #          br(),
                                #          h4("Notes"),
                                #          tags$li("Order = The sample completeness order of q between 0 and 3 in increments of 0.25 (or those orders you type in the input window)."),
                                #          tags$li("Estimate = sample completeness estimated."),
                                #          tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for the sample completeness of order q at the specified level (with a default value of 0.95).")
                                # 
                                # 
                                # ),
                                tabPanel("Sample Completeness Curve (estimated)",icon=icon("picture-o"),
                                         h4("Estimated Sample Completeness Curve"),
                                         plotOutput("plotprofile_sc"),
                                         downloadButton('downloadplotprofile_sc','Save as PNG File'),
                                         h4("Estimated Sample Completeness for Each Order"),
                                         tableOutput("scprofile_table"),
                                         downloadButton('downloadprofile2_sc','Save as CSV File'),
                                         
                                         br(),
                                         br(),
                                         h4("Notes"),
                                         tags$li("Order.q = The sample completeness order of q between 0 and 3 in increments of 0.25 (or those orders you type in the input window)."),    
                                         tags$li("Estimate = estimated sample completeness ."),
                                         tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for the sample completeness of order q at the specified level (with a default value of 0.95).")
                                         
                                         
                                ),
                                tabPanel("Introduction",icon=icon("question-circle"),
                                         includeMarkdown("source/about.md")
                                ),
                                tabPanel("User Guide",icon=icon("question-circle"),
                                         br(),
                                         tags$a("User Guide Link",href="http://chao.stat.nthu.edu.tw/wordpress/wp-content/uploads/software/iNEXTOnline_UserGuide.pdf")
                                )

                              )
             ),
             conditionalPanel("input.index=='iNEXT'",
                         tabsetPanel(
               tabPanel("Data Summary",icon=icon("list-alt"),
                        h3("Basic data information"),
                        tableOutput("table"),
                        br(),
                        h4("Notes"),
                        conditionalPanel("input.datatype=='abundance'",
                                         tags$li("n = number of observed individuals in the reference sample (sample size)."),
                                         tags$li("S.obs = number of observed species in the reference sample."),
                                         tags$li("C.hat = estimator of the sample coverage of the reference sample"),
                                         tags$li("f1-f10 = the first ten species abundance frequency counts in the sample.")),
                        
                        conditionalPanel("input.datatype=='incidence_freq'",
                                         tags$li("T = number of observed sampling units in the reference sample (sample size for incidence data)."),
                                         tags$li("U = total incidence in the reference sample."),
                                         tags$li("S.obs = number of observed species"),
                                         tags$li("C.hat = estimator of the sample coverage suggetsed by Chao et al. (2013)"),
                                         tags$li("Q1-Q10 = the first ten speceis incidence frequency counts in the refernce sample."))
                       ),
               
               tabPanel("Rarefaction and Extrapolation",icon=icon("picture-o"),
                        h4("Rarefaction and Extrapolation"),
                        tableOutput("RE_table"),
                        downloadButton('downloadprofile','Save as CSV File'),
                        
                        br(),
                        br(),
                        h4("Notes"),
                        conditionalPanel("input.datatype=='abundance'",
                                         tags$li("m = sample size for which diversity estimates of order q are computed; by default setting (in the left hand side of the screen), m represents the sample size for each of the 40 knots between 1 and the default endpoint (double the reference sample size). On the 'General Setting', you can also either specify the endpoint and knots or specify the samples sizes for which you like to calculate diversity estimates."),    
                                         tags$li("method = interpolated, observed, or extrapolated, depending on whether the size m is less than, equal to, or greater than the reference sample size."),
                                         tags$li("order = the diversity order of q you selected in the 'General Setting' on the left hand side of the screen."),
                                         tags$li("qD = the estimated diversity of order q for a sample of size m."),
                                         tags$li("SC = the estimated sample coverage for a sample of size m."),
                                         tags$li("qD.LCL, qD.UCL = the bootstrap lower and upper confidence limits for the diversity of order q at the specified level in the setting (with a default value of 0.95)."),
                                         tags$li("SC.LCL, SC.UCL = the bootstrap lower and upper confidence limits for the expected sample coverage at the specified level in the setting (with a default value of 0.95).")
                                         ),
                        
                        conditionalPanel("input.datatype=='incidence_freq'",
                                         tags$li("t = sample size for which diversity estimates of order q are computed; by default setting (in the left hand side of the screen), t represents the sample size for each of the 40 knots between 1 and the default endpoint (double the reference sample size). On the 'General Setting', you can also either specify the endpoint and knots or specify the samples sizes for which you like to calculate diversity estimates."),    
                                         tags$li("method = interpolated, observed, or extrapolated, depending on whether the size t is less than, equal to, or greater than the reference sample size."),
                                         tags$li("order = the diversity order of q you selected in the 'General Setting' on the left hand side of the screen."),
                                         tags$li("qD = the estimated diversity of order q for a sample of size t."),
                                         tags$li("SC = the estimated sample coverage for a sample of size t."),
                                         tags$li("qD.LCL, qD.UCL = the bootstrap lower and upper confidence limits for the diversity of order q at the specified level in the setting (with a default value of 0.95)."),
                                         tags$li("SC.LCL, SC.UCL = the bootstrap lower and upper confidence limits for the expected sample coverage at the specified level in the setting (with a default value of 0.95).")
                       )),
               
               tabPanel("Figure Plots",icon=icon("picture-o"),
                        h4("(1) Sample-size-based rarefaction and extrapolation sampling curve"),
                        plotOutput("myplotprofilesize"),
                        #h5("Species richness estimates for a rarefied and extrapolated sample with sample size up to double the reference sample size."),
                        downloadButton('downloadplotsize','Save as PNG File'),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h4("(2) Sample completeness curve"),
                        plotOutput("myplotprofilesizeC"),
                        #h5("Sample completeness (as measured by sample coverage) with respect to sample size. This curve provides a bridge between sample-size- and coverage-based rarefaction and extrapolation."),
                        downloadButton('downloadplotsizeC','Save as PNG File'),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h4("(3) Coverage-based rarefaction and extrapolation sampling curve"),
                        plotOutput("myplotprofileC"),
                        #h5("species richness estimates for rarefied sample and extrapolated sample with sample coverage up to double the reference sample size."),
                        downloadButton('downloadplotC','Save as PNG File')
                       ),
               tabPanel("Introduction",icon=icon("question-circle"),
                        includeMarkdown("source/about.md")    
               ),
               tabPanel("User Guide",icon=icon("question-circle"),
                        br(),
                        tags$a("User Guide Link",href="http://chao.stat.nthu.edu.tw/wordpress/wp-content/uploads/software/iNEXTOnline_UserGuide.pdf")
               )
                       )
             ),
             conditionalPanel("input.index=='Profile'",
                              tabsetPanel(
                                
                                tabPanel("Diversity Profile (estimated)",icon=icon("picture-o"),
                                         h4("Estimated Diversity Profiles"),
                                         plotOutput("plotprofile"),
                                         downloadButton('downloadplotprofile','Save as PNG File'),
                                         h4("Estimated Diversity and Entropy for Each Order"),
                                         tableOutput("Dprofile_table_diversity"),
                                         tableOutput("Dprofile_table_entropy"),
                                         downloadButton('downloadprofile2','Save as CSV File'),
                                         
                                         br(),
                                         br(),
                                         h4("Notes"),
                                         tags$li("Order = the diversity order of q between 0 and 3 in increments of 0.25 (or those orders you type in the input window)."),    
                                         tags$li("Target = diversity or entropy."),
                                         tags$li("Estimate = diversity or entropy estimated by Chao and Jost (2015) method."),
                                         tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for the diversity or entropy of order q at the specified level (with a default value of 0.95).")
                                         
                                         
                                ),
                                
                                tabPanel("Diversity Profile (empirical)",icon=icon("picture-o"),
                                         h4("Estimated Diversity Profiles"),
                                         plotOutput("plotprofile.em"),
                                         downloadButton('downloadplotprofile.em','Save as PNG File'),
                                         h4("Estimated Diversity and Entropy for Each Order"),
                                         tableOutput("Emperical_table_diversity"),
                                         tableOutput("Emperical_table_entropy"),
                                         downloadButton('downloadprofile.em','Save as CSV File'),
                                         
                                         
                                         br(),
                                         br(),
                                         h4("Notes"),
                                         tags$li("Order = the diversity order of q between 0 and 3 in increments of 0.25 (or those orders you type in the input window)."),    
                                         tags$li("Target = diversity or entropy."),
                                         tags$li("Empirical = observed diversity or entropy."),
                                         tags$li("LCL, UCL = the bootstrap lower and upper confidence limits for the diversity or entropy of order q at the specified level (with a default value of 0.95).")
                                ),
                                tabPanel("Introduction",icon=icon("question-circle"),
                                         includeMarkdown("source/about.md")
                                ),
                                tabPanel("User Guide",icon=icon("question-circle"),
                                         br(),
                                         tags$a("User Guide Link",href="http://chao.stat.nthu.edu.tw/wordpress/wp-content/uploads/software/iNEXTOnline_UserGuide.pdf")  
                                )
                               
                              )
                              )
             
    
)))))

