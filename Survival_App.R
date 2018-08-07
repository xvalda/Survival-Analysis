library(shiny); library(ggplot2); library(dplyr); library(survival); library(survminer); library(DT); library(shinythemes)
data(veteran)
veteran$trt <- factor(veteran$trt, labels = c("standard", "test"))
veteran$prior <- factor(veteran$prior, labels = c("no", "yes"))
veteran$status <- factor(veteran$status, labels = c("Censored", "Not_Censored"))
n_total <- nrow(veteran)

ui <- fluidPage(style = "background-color:#00cccc;",
  titlePanel("Survival Analysis with Veteran Dataset", windowTitle = "Survival Analysis"),
  tabsetPanel(
    tabPanel("1. Introduction & Dataset", 
             fluidRow(style = "background-color:#e6f7ff;", 
               column(width = 4, offset = 1,
               br(),
               p(tags$b("Survival analysis applies to datasets where we measure events occuring over time, 
                and gathers a set of methods to answer some questions such as:")),
                tags$li("how much time it takes for an event to occur"),
                tags$li("what is the probability for a patient to survive a certain amount of time, given a condition"),
                tags$li("are there statistically significant differences in survival time between different patients groups"),
               br(), br()
             ),
             column(width = 4, offset = 1,
                    h3("About this application"),
                    tags$li(tags$b("Tab 1:"), "Introduction and presentation of the dataset, details about the dataset and codebook"),
                    tags$li(tags$b("Tab 2:"), " Basic data exploration, the user can have basic summary statistics of the variables and a few 
                            plots (histogram and scatterplot) in order to get a better feel of the data"),
                    tags$li(tags$b("Tab 3"), " Survival Analysis: Kaplan Meier Survival Curves"),
                    tags$li(tags$b("Tab 4"), " Full Model Summary, full model output"),
                    tags$li(tags$b("Tab 5"), " References and contact, intuition about survival analysis, references/credits, contact details"), br()
                    )
             
             ),
             fluidRow(style = "height:10px"),
             fluidRow(style = "background-color:#e6ffff;",
               column(width = 4, offset = 1,
                      br(),
                      h3("Data source: "), 
                      p("We use the 'veteran' dataset from the Survival library in R. The data comes from a randomized experiment 
                        between two treatment groups for lung cancer, it contains 137 observations of 8 variables and we recoded some of the 
                        variables into factors for better readability: "),
                      tags$li(tags$b("trt:"), " type of treatment, 1 = standard, 2 = test"),
                      tags$li(tags$b("celltype:"), " factor describing the type of cell: 1=squamous, 2=smallcell, 3=adeno, 4=large"),
                      tags$li(tags$b("time:"), " survival time (from start of study to death), in days"),
                      tags$li(tags$b("status:"), " censoring status, 0 = patient death was not observed (survival time was censored), 1 = patient death was observed"),
                      tags$li(tags$b("karno:"), " Karnofsky performance score (quantifies cancer patients' general well-being and activities of daily life, 0 = Dead to 100 = Normal)"),
                      tags$li(tags$b("diagtime:"), " time from diagnosis to randomisation, in months"),
                      tags$li(tags$b("age:"), " age of the patient in years"),
                      tags$li(tags$b("prior:"), " 0 = no prior therapy, 10 = prior therapy")
               ),
               column(width = 4, offset = 1,
                      br(),
                      h3("DATA"),
                      DT::dataTableOutput(outputId = "veterantable"), br()
               )
             )
             ),
    
    tabPanel("2. Basic Data Exploration", 
             
               fluidRow(style = "background-color:#e6f7ff;", 
                 column(width = 3, 
                        br(),
                 selectInput(inputId = "x",
                             label = "Select variable for X-Axis (histogram and scatterplot):",
                             choices = names(veteran), 
                             selected = "karno"),
                 selectInput(inputId = "col",
                             label = "Select Variable for color:",
                             choices = names(veteran), 
                             selected = "trt")
               ),
               column(width = 4,
                br(), 
                textOutput(outputId = "caption_histogram"), 
                plotOutput(outputId = "plot2", height = 350), br()
               ),
               column(width = 5,
                br(), 
                textOutput(outputId = "summary_stats"), br(), br(),
                verbatimTextOutput(outputId = "summarytable")
                      )
             ),
             fluidRow(style = "height:10px"),
             fluidRow(style = "background-color:#e6ffff;",
               column(width = 3,
                      br(), br(), br(),
                      tags$em("Please 'brush-select' (holding the mouse button) some of the points on the scatterplot to display the data that will appear 
                              in the right pane"),
                      br(), br(), 
                      selectInput(inputId = "y",
                                  label = "Select variable for Y-Axis (for scatterplot only):",
                                  choices = names(veteran), 
                                  selected = "celltype"),
                      sliderInput(inputId = "alpha",
                                  label = "Alpha:",
                                  min = 0, max = 1, step = 0.1, 
                                  value = 0.9)
                      ),
               
               column(width = 4,
                      br(),
                      textOutput(outputId = "caption_scatterplot"),
                      plotOutput(outputId = "plot1", brush = "plot_brush", height = 350), br()
                      ),
               column(width = 5,       
                      br(),
                      dataTableOutput(outputId = "veterantable2")
               )
             )
             ),
    
    tabPanel("3. Survival Curve",
    fluidRow(style = "height:10px", style = "background-color:#e6ffff;"),
    fluidRow(style = "background-color:#e6ffff;",
      column(width = 3, 
             br(),
             wellPanel(
               h3("Explanatory variables:"),
               selectInput(inputId = "pred_vars", label = "Select Variable:", choices = c("trt", "celltype", "karno", "prior"), 
                           selected = "trt")
             ),
             wellPanel(
               h3("Plotting options:"),
               checkboxInput(inputId = "show_pvalue", label = "Show P-Value:", value = TRUE),
               checkboxInput(inputId = "show_ci", label = "Show Confidence Interval:", value = TRUE),
               selectInput(inputId = "fun_trans", label = "Select transformation function:", choices = c("none", "log", "event", "cumhaz"),
                           selected = "none")
             ),
             wellPanel(
               h3("Filters"),
               selectInput(inputId = "filt_trt", label = "Select treatment types:", choices = levels(veteran$trt), 
                           selected = c("standard", "test"), multiple=TRUE),
               selectInput(inputId = "filt_celltype", label = "Select cell types:", choices = levels(veteran$celltype), 
                           selected = levels(veteran$celltype), multiple=TRUE),
               selectInput(inputId = "filt_prior", label = "Select prior treatment status:", choices = levels(veteran$prior), 
                           selected = levels(veteran$prior), multiple=TRUE),
               sliderInput(inputId = "filt_age", label = "Modify Age Range", min = 34, max = 81, value  = c(34,81))
             )
      ),
      column(width = 6, 
             br(),
             h3("Short Summary Table of the Survival Curve"), h5(em("(full summary available in 'Tab 4. Full Model Summary')")),
             tableOutput(outputId = "km_summary"), br(),
             plotOutput(outputId = "km_curve", height = 650),
             br()
      ),
      column(width = 3, style = "background-color:#e6f7ff;",
             br(),
             h3("Reading and interpreting the output:"),
             h4("Summary Table"),
             tags$li(tags$b("records:"), "number of observations or cases for each class"),
             tags$li(tags$b("events:"), "number of events that occurred "),
             tags$li(tags$b("mean:"), "mean survival time"),
             tags$li(tags$b("median:"), "median of the survival time"),
             tags$li(tags$b("0.95 LCL:"), "lower confidence limit (confidence interval = 95%) for the curve"),
             tags$li(tags$b("0.95 UCL:"), "upper confidence limit (confidence interval = 95%) for the curve"),
             br(),
             h4("Plot"),
             tags$li(tags$b("Strata:"), "number of subjects in each stratum. If strata is not NULL, there are multiple curves in the result. The levels of strata (a factor) are the labels for the curves"),
             tags$li(tags$b("Number at risk (or n.risk):"), "number of subjects at risk at time t (especially significant in the full output)"),
             tags$li(tags$b("n.censor:"), "the number of censored subjects, who exit the risk set, without an event, at time t"),
             br(),
             h4("p-value of the log-rank test"),
             tags$li("We compare 2 or more survival curves with the null hypothesis that curves are identical"),
            tags$li("if p<0.05, we reject the null hyopthesis and estimate that there is a significant statistical evidence that the curves are different"),
            tags$li("if the p-value > 0.05, we fail to reject Ho and estimate that the difference between the curves is not significant. "),
             verbatimTextOutput(outputId = "surv_diff_table"), br()
             
      )
      
    )
  ),
    
    tabPanel("4. Full Model Summary", style = "background-color:#000000;",
             br(), 
             p("You'll find below the full summary of the model. The variables are explained in Tab3.", style = "color:#ffffff;"),
             verbatimTextOutput(outputId = "fullsummary"), br()
             ),

    tabPanel("5. References & Contact",
             fluidRow(style = "background-color:#e6f7ff;",
                      column(width = 4, offset = 1, 
                             h4("Potential further developments are:"), 
                             tags$li("archiving more datasets"), 
                             tags$li("allowing user to import own dataset, user will need to enter the name of the outcome variable"), 
                             tags$li("more sophisticated plotting options"), 
                             tags$li("more algorithms (Cox Proportional Hazard Model)"), 
                             tags$li("option to export a pdf report"),
                             h4("Contact"), 
                             p("For questions, requests for further developments, maintenance issues or any comment, please feel free to reach out, indicating it relates to the 'Survival Shiny App':   "), 
                             tags$li("xavier@measuringsocial.com   "), 
                             tags$li("www.linkedin.com/in/xavier-valdayron-9707231"), br() 
                             
                      ),
                      column(width = 4, offset = 1,
                             h4("References"), 
                             p("The files for this project:"), 
                             tags$li(a(href = "RMarkdown project file", "http://rpubs.com/xvalda/survival")),
                             tags$li(a(href = "Github", "https://github.com/xvalda/Survival-Analysis")),
                             br(),
                             
                             p("In order to compile this introduction to survival analysis, We used resources, some of them extensively:    "), 
                             tags$li(a("http://www.sthda.com/english/wiki/cox-proportional-hazards-model   ")), 
                             tags$li(a("http://www.sthda.com/english/wiki/cox-model-assumptions   ")), 
                            tags$li(a("http://www.statisticshowto.com/survival-analysis/   ")), 
                                    tags$li(a("https://www.openintro.org/download.php?file=survival_analysis_in_R   ")), 
                                            tags$li(a("http://www.ms.uky.edu/~mai/Rsurv.pdf   "))
                      )
             ),
             fluidRow(style = "height:10px"),
             fluidRow(style = "background-color:#e6ffff;", 
              column(width = 4, offset = 1,
                  h3("Intuition about survival analysis"), 
                  p("We want to estimate the time to reach a specific event."), 
                  p("Although very much used in clinical studies where the event is death or recurrence of a disease across two or more treatment groups, there are business/economical applications such as time from manufacturing a new machine component to the failure of this component, or lifetime of two different models of light bulbs, or estimating the time people remain unemployed, ."), 
                  h4("1. Event"), 
                  p("The event is the endpoint in the study, the specific outcome we want to measure: death, relapse, mechanical component failure, ."), 
                  h4("2. Censored Data"), 
                  p("This refers to incomplete data, the event did not occur for a subject during the time of the study, there are several cases:"), 
                  p("- patient did not experience any event during the study, and we do not know if the event occured (for instance, we do not know if the patient ultimately survived or not after the study)"), 
                  p("- Right censored subjects: the patient withdrew from the trial or data is lost for some reason (follow-up didn't occur on the patient, or the patient experienced a different event)"), 
                  p("All patients not experiencing the event during the time of the study will be censored at the latest recorded time point."), 
                  h4("3. Kaplan-Meier"), 
                  p("Main purpose: visualing survival curves, works well with categorical data, for numerical data we'll use the Cox proportional hazard models"), 
                  p("After releasing similar findings, Edward Kaplan and Paul Meier published their joint work in 1958 about the non-parametric statistic (i.e. does not assume any underlying probability distribution)."), 
                  p("Kaplan-Meier statistic measures the probability that a patient will survive past a specific point in time."), 
                  p("At t = 0, the statistic is 1 (or 100%)."), 
                  p("When t increases infinitely, the statistic becomes 0."), 
                  p("The plot of the KM estimator is a series of decreasing horizontal steps, approaching the true survival function."), 
                  p("The survival probability of surviving after time = t is noted S(t). It is the product of all the prior survival probabilities up to time = t."), 
                  p("This is based on conditional probabilities, each new proportion conditional on the previous proportions.")
               ),
               column(width = 4, offset = 1,
                  br(),
                  h4("4. Log-Rank Test"), 
                  p("This test is performed on the curves of the Kaplan-Meier method."), 
                  p("For two (or more) different survival curves, it tests the null hypothesis that both curves are equal. If the p-value is below the significance level (generally alpha = 0.05), then we have convincing statistical evidence that at least two curves differ."), 
                  h4("5. Cox proportional hazard models"), 
                  p("Main purpose: describing the simultaneous effect of several variables on the rate of a particular event happening at a specific point in time."), 
                  p("The Cox model is defined with the hazard function, h(t): it describes the probability of a hazard of a subject to survive to time = t. The function returns a proportion, from 0 to 1."), 
                  p("It measures the instaneous risk of death. It has a memory-less property, the likelihood of something happening at time = t has no relation to what happened in the past. The function at year y applies to all subjects alive that year, without taking into account who died in previous years."), 
                  p("The exponential survival distribution models the time until the event (component failure, .)."), 
                  p("Covariates are the predictors we use in the model that looks pretty much like a multiple linear regression."), 
                  h4("6. Difference between survival and hazard functions"), 
                  p("The survivor function describes the probability of 'not having an event', whereas the hazard function describes the probability of the event occurring."), 
                  p("The survival function is the probability that a subject survives from the time origin of the study to a specified future time t."), 
                  p("The hazard function is the probability that a subject under observation at a time t has an event at that time.") 
                  )
              )
         )
    )
)

#SERVER######################
server <- function(input, output){
  
      
              
#tab 2 - basic data visualization  
  maintable <- reactive({
    maintable <- veteran
  })
  
    output$plot1 <- renderPlot({
    ggplot(data = veteran, aes_string(x = input$x, y = input$y, color = input$col, fill = input$col)) +
      geom_point(alpha = input$alpha) 
  })
  output$plot2 <- renderPlot({
    ggplot(data = veteran, aes_string(x = input$x, fill = input$col)) +
      geom_histogram(stat = "count", bins = 15)
  })
  output$veterantable <- DT::renderDataTable({
      DT::datatable(data = maintable(),
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  })

  output$summary_stats <- renderText({
      paste("Summary statistics of the", input$x, "variable: ")
  })
  output$caption_histogram <- renderText({
    paste("Histogram of the", input$x, "variable: ")
  })
  output$caption_scatterplot <- renderText({
    paste0("Scatterplot of x = ", input$x, " and y = ", input$y, ":")
  })
  
  output$veterantable2 <- DT::renderDataTable({
      brushedPoints(veteran, input$plot_brush)
  })
  output$summarytable <- renderPrint({
     x <- veteran %>% pull(input$x)
     summ <- summary(x)
     print(summ, digits = 3)
  })

#for tab 3 - survvival analysis
  subset_km <<- reactive({
    veteran %>% filter(celltype %in% input$filt_celltype) %>% 
      filter(trt %in% input$filt_trt) %>% 
      filter(prior %in% input$filt_prior) %>%
      filter(age >= min(input$filt_age) & age <= max(input$filt_age))
  })
  
  fit_km <- reactive({
    if(input$pred_vars == "trt"){
      survfit(Surv(time, as.numeric(status)) ~ trt, data=subset_km())
    }else if(input$pred_vars == "celltype"){
      survfit(Surv(time, as.numeric(status)) ~ celltype, data=subset_km())
    }else if(input$pred_vars == "karno"){
      survfit(Surv(time, as.numeric(status)) ~ as.factor(cut(veteran$karno, breaks = 5)), data=subset_km())
    }else if(input$pred_vars == "prior"){
      survfit(Surv(time, as.numeric(status)) ~ prior, data=subset_km())
    }
  })
  
  output$km_curve <- renderPlot({
    if(input$fun_trans == "none"){
      ggsurvplot(
        fit = fit_km(),           #survival model we want to plot 
        pval = input$show_pvalue, #displays p-value of log-rank test, if p-value < 0.05, then the difference between the two 
        #curves are statistically significant
        conf.int = input$show_ci, #plots a confidence interval for each curve
        xlab = "Time in days",
        break.time.by = 150,      # break X axis in time intervals by 100.
        ggtheme = theme_light(),  # customize theme with a grid for better readability 
        risk.table = "abs_pct",   # absolute number and percentage at risk
        risk.table.y.text.col = T,# colour risk table text annotations
        risk.table.y.text = FALSE,# show bars instead of names in text annotations
        # in legend of risk table.
        ncensor.plot = TRUE,      # plot the number of censored subjects at time t
        surv.median.line = "hv"   # add the median survival pointer
      )
    }else if(input$fun_trans == "log"){
      ggsurvplot(
        fit = fit_km(), pval = input$show_pvalue, conf.int = input$show_ci, xlab = "Time in days",break.time.by = 150,
        ggtheme = theme_light(),  risk.table = "abs_pct", risk.table.y.text.col = T, risk.table.y.text = FALSE,ncensor.plot = TRUE, 
        surv.median.line = "hv", fun = "log")
    }else if(input$fun_trans == "event"){
      ggsurvplot(
        fit = fit_km(), pval = input$show_pvalue, conf.int = input$show_ci, xlab = "Time in days",break.time.by = 150,
        ggtheme = theme_light(),  risk.table = "abs_pct", risk.table.y.text.col = T, risk.table.y.text = FALSE,ncensor.plot = TRUE, 
        surv.median.line = "hv", fun = "event")
    }else if(input$fun_trans == "cumhaz"){
      ggsurvplot(
        fit = fit_km(), pval = input$show_pvalue, conf.int = input$show_ci, xlab = "Time in days",break.time.by = 150,
        ggtheme = theme_light(),  risk.table = "abs_pct", risk.table.y.text.col = T, risk.table.y.text = FALSE,ncensor.plot = TRUE, 
        surv.median.line = "hv", fun = "cumhaz")
    }
  })
  #km_summary useed in tab 3 and 4
  km_summary <- reactive({
    summary(fit_km())
  })
  
  output$km_summary <- renderTable(km_summary()$table, rownames = TRUE)
  
  #survdiff - log rank test
  surv_diff <- reactive({
    if(input$pred_vars == "trt"){
      survdiff(Surv(time, as.numeric(status)) ~ trt, data=subset_km())
    }else if(input$pred_vars == "celltype"){
      survdiff(Surv(time, as.numeric(status)) ~ celltype, data=subset_km())
    }else if(input$pred_vars == "karno"){
      survdiff(Surv(time, as.numeric(status)) ~ as.factor(cut(veteran$karno, breaks = 5)), data=subset_km())
    }else if(input$pred_vars == "prior"){
      survdiff(Surv(time, as.numeric(status)) ~ prior, data=subset_km())
    }
  })
  
  output$surv_diff_table <- renderPrint({surv_diff()})
  
  #tab 4 - Full Summary
  output$fullsummary <- renderPrint({km_summary()})

  
}

shinyApp(ui = ui, server = server)
