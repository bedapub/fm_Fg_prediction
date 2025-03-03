#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Define the libraries #####

library (tidyverse)
library (dplyr)
library (ggplot2)
library (table1)
library (ggpubr) 
library(caTools) 
library(shiny)
library(shinyjs)
library(lhs)
library(DT)


# Define functions for calculations

source("./math_functions.R")

# Define functions for plots

source("./plots.R")

# Define functions for updating the UI based on the CI, tv, and sigma

source("./Ui_functions.R")



# Read the text files as respective instructions
info_param_uncert_tab <- readLines("info_param_uncert_tab.txt")
info_advance_tab <- readLines("info_advanced_settings.txt")

equations_text <- paste(readLines("equations.txt"), collapse = "\n")

# Convert the text into HTML list items
info_param_uncert_tab_html <-  info_param_uncert_tab


options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI for application that draws a histogram


ui <- function() {
  fluidPage(
    useShinyjs(),  # Initialize shinyjs
    tags$img(src = "Roche_logo.png"),
    tags$img(src = "logo_MU.jpg"),
    navbarPage( id = "main_navbar",  # Add an id to the navbarPage

      title = span("fm and FG estimation", style = "background-color: #DEEBF7"),
      tabPanel(
        "Parameters & Uncertainty",
        sidebarPanel( width = 4,  # Adjust the width here (default is 4)
          style = "background-color:#fffdf7;",
          fluidRow(
            tags$h2(class = "text-center", "Input Parameters"),
            column(
              3,
              tags$div(
                style = "display: flex; align-items: center;",
                tags$h3(style = "margin: 1;", "Typical Value"),
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2; margin-left: 5px;",
                  title = "Typical value of each parameter"
                )
              ),              numericInput(
                "CLp",
                label = tags$span(
                  "IV Plasma Clearance (L/h)",
                ),
                min = 0, max = 100, value = 37.1, step = 1.2
              ),
              numericInput(
                "Rb",
                label = tags$span(
                  "Blood to Plasma Ratio",
                ),
                min = 0.4, max = 5, value = 1, step = 0.1
              ),
              numericInput(
                "Vlk",
                label = tags$span(
                  "Distribution volume (L)"
                ),
                min = 0, max = 10, value = 641, step = 0.01
              ),
              numericInput("CLR",
                                  "Non hepatic clearance (L/h)",
                                  min = 0,
                                  max = 50,
                                  value = 0)
            ),
            
            column(
              2,
              tags$div(
                style = "display: flex; flex-direction: column; align-items: flex-start;",
                tags$h3(style = "margin: 1;", "Uncertainty"),
                
                tags$div(
                  style = "margin-top: 45px;",  # Adjust the margin as needed
                  numericInput("CLp_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.1)
                ),
                
                tags$div(
                  style = "margin-top: 22px;",  # Adjust the margin as needed
                  numericInput("Rb_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.1)
                ),
                
                tags$div(
                  style = "margin-top: 18px;",  # Adjust the margin as needed
                  numericInput("Vlk_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.1)
                ),
                
                tags$div(
                  style = "margin-top: 20px;",  # Adjust the margin as needed
                  numericInput("CLR_var", "σ", min = 0, max = 1, value = 0.1)
                )
              )
            ),
            column(
              3,
              tags$div(
                style = "display: flex; align-items: center;",
                tags$h3(style = "margin: 1;", br()),
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2; margin-left: 55px;",
                  title = "-sigma (standard deviation) \n-CI (confidence interval) with 95% CL in the linear domain"
                )
              ),
              #  textInput("CLp__int", "CI", value = NaN, placeholder = "Enter confidence level"),
              #  textInput("Rb_int", "CI", value = NaN, placeholder = "Enter lower bound of CI"),
              #  textInput("Vlk_int", "CI", value = NaN, placeholder = "Enter upper bound of CI"),
               # textInput("CLR_int", "CI", value = NaN, placeholder = "Enter sample size"),
            ),
            column(
              1,
              class = "text-center",
              tags$div(
                style = "margin-top: 200px;",  # Add margin-top property to create space below
                tags$style(".checkbox-inline input[type='checkbox'] { transform: scale(100); }"),
                actionButton("activateButton_drug_param", "Reset", disabled = TRUE)
              )
            )
          ),
          fluidRow(
            style = "background-color:#eaf4f4;",
            column(
              3,
              div(style = "margin-top: 0.48cm;",
                  numericInput(
                    "AUCR",
                    label = tags$span(
                      "AUCR",
                      tags$i(
                        class = "glyphicon glyphicon-info-sign",
                        style = "color:#0072B2;",
                        title = "AUCR ratio no Inhibition/Inhibition"
                      )
                    ),
                    min = 5, max = 20, value = 6.80, step = 0.5
                  )
              ),
                div(style = "margin-top: 0.55cm;",
                    numericInput(
                      "CMXR",
                      label = tags$span(
                        "CMAXR",
                        tags$i(
                          class = "glyphicon glyphicon-info-sign",
                          style = "color:#0072B2;",
                          title = "CMAX ratio no Inhibition/Inhibition"
                        )
                      ),
                      min = 1, max = 20, value = 2.41, step = 0.5
                    )  
                ),
              numericInput(
                "tmax.po",
                label = tags$span(
                  "Tmax PO (h)",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "Tmax oral absorbtion"
                  )
                ),
                min = 0, max = 20, value = 1.94, step = 0.1
              )
            ),
            column(
              2,
              div(style = "margin-top: 0.48cm;",
                  numericInput("AUCR_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.01)
              ),
              div(style = "margin-top: 0.55cm;",
                  numericInput("CMXR_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.01)
              ),              
              numericInput("tmaxPO_var", " \u03C3", min = 0, max = 1, value = 0.1, step = 0.01)
            ),
            column(
              3,    tags$script(HTML("
                                  $(document).on('click', '#confirm_AUCR_int', function() {
                                    $('#AUCR_int').addClass('blue-text');
                                  });
                                  $(document).on('input', '#AUCR_var', function() {
                                    $('#AUCR_int').removeClass('blue-text');
                                  });
                                  $(document).on('input', '#AUCR', function() {
                                    $('#AUCR_int').removeClass('blue-text');
                                  });
                                ")), 
              tags$style(HTML("
                                  .input-group {
                                    position: relative;
                                  }
                                  .input-group .form-control {
                                    padding-right: 40px; /* Adjust padding to make space for the button */
                                  }
                                  .input-group .btn-enter {
                                    position: absolute;
                                    right: 10px;
                                    top: calc(100% + 5px); /* Move the button 0.5 cm (5px) below the input */
                                    padding: 2px 5px;
                                    font-size: 10px;
                                    line-height: 1;
                                    z-index: 2;
                                  }
                                  .blue-text {
                                    color: blue;
                                  }
                                ")),
              div(
                class = "input-group",
                textInput("AUCR_int", "CI", value = "5.59, 8.27"),
                actionButton("confirm_AUCR_int", "Enter", class = "btn-enter"),
              ),
              htmlOutput("message_sigma_AUCR"),  # Add textOutput to display the message
              
              tags$script(HTML("
                                  $(document).on('click', '#confirm_CMXR_int', function() {
                                    $('#CMXR_int').addClass('blue-text');
                                  });
                                  $(document).on('input', '#CMXR_var', function() {
                                    $('#CMXR_int').removeClass('blue-text');
                                  });
                                  $(document).on('input', '#CMXR', function() {
                                    $('#CMXR_int').removeClass('blue-text');
                                  });
                                ")),
              tags$style(HTML("
                                  .input-group {
                                    position: relative;
                                    margin-top: 5mm; /* Move the input group 1 cm (10mm) below its current position */

                                  }
                                  .input-group .form-control {
                                    padding-right: 40px; /* Adjust padding to make space for the button */
                                  }
                                  .input-group .btn-enter {
                                    position: absolute;
                                    right: 10px;
                                    top: calc(100% + 5px); /* Move the button 0.5 cm (5px) below the input */
                                    padding: 2px 5px;
                                    font-size: 10px;
                                    line-height: 1;
                                    z-index: 2;
                                  }
                                  .blue-text {
                                    color: blue;
                                  }
                                ")),
              div(
                class = "input-group",
                textInput("CMXR_int", "CI", value = "1.98, 2.93"),
                actionButton("confirm_CMXR_int", "Enter", class = "btn-enter")
              ),
              # textInput("tmaxPO_int", "CI", NaN)
              htmlOutput("message_sigma_CMXR")  # Add textOutput to display the message
              
            ),
            column(
              1,
              class = "text-center",
              tags$div(
                style = "margin-top: 130px;",  # Add margin-top property to create space below
                tags$style(".checkbox-inline input[type='checkbox'] { transform: scale(100); }"),
                actionButton("activateButton_ddi_param", "Reset", disabled = TRUE)
              )
            )
          )
        )
      ),
      tabPanel(
        "Advanced settings",
        sidebarPanel(
          fluidRow(
            column(
              6,
              numericInput(
                "tHeight",
                label = tags$span(
                  "Height",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "cm"
                  )
                ),
                min = 50, max = 210, value = 175, step = 1
              ),
              numericInput(
                "tAge",
                label = tags$span(
                  "Age",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "typical value for the population age"
                  )
                ),
                min = 1, max = 90, value = 40, step = 1
              ),
              numericInput(
                "WT_M",
                label = tags$span(
                  "Body Weight (Kg) Male",
                ),
                min = 50, max = 200, value = 81, step = 1
              ),
              numericInput(
                "WT_F",
                label = tags$span(
                  "Body Weight (Kg) Female",
                ),
                min = 50, max = 200, value = 69, step = 1
              ),
              sliderInput(
                "Sex_ratio",
                label = tags$span(
                  "Sex ratio",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "female/male"
                  )
                ),
                min = 0, max = 1, value = 0.5, step = 0.1
              ),
              sliderInput(
                "Observations",
                label = tags$span(
                  "Number of samplings",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "number of repetitions"
                  )
                ),
                min = 100, max = 1000, value = 100, step = 10
              ),
              sliderInput(
                "Points",
                label = tags$span(
                  "Points",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "NUmber of steps between 0 and 1"
                  )
                ),
                min = 0.0001, max = 0.01, value = 0.001, step = 0.00001
              ),
              numericInput(
                "inh",
                label = tags$span(
                  "Hepatic Inhibition %",
                  tags$i(
                    class = "glyphicon glyphicon-info-sign",
                    style = "color:#0072B2;",
                    title = "% of inhibition"
                  )
                ),
                min = 1, max = 99, value = 95, step = 1
              ),
              numericInput(
                "inhGI",
                 label = tags$span(
                   "Gut Inhibition %",
                   tags$i(
                     class = "glyphicon glyphicon-info-sign",
                     style = "color:#0072B2;",
                     title = "% of inhibition"
                   )
                 ),
                   min = 0, max = 100, value = 100
                ),
              checkboxInput("set_seed", "SEED", value = TRUE, width = NULL),
              h4("Plot settings"),
              checkboxInput("Show_seg_var", "Show segments variability", FALSE),
              ),
            column(
              6,
              numericInput("Height_var", '\u03C3', min = 0, max = 1, value = 0.1, step = 0.1),
              numericInput("Age_var", '\u03C3', min = 0, max = 1, value = 0.1, step = 0.1),
              #numericInput("WT_var", " \u03C3", min = 0.01, max = 1, value = 0.1, step = 0.01), no longer necessary, LHS is applied! 
            )
          )
        )
      ),
      tabPanel(
        "Model Info",
        mainPanel(
          h2("MODEL INFORMATION"),
          radioButtons(
            "Info_Selection",
            "Info",
            choices = list(
              "Method" = "Method",
              "Instructions" = "Instructions",
              "Authors list" = "Authors_list"
            ),
            inline = T,
            selected = "Method"
          ),
          conditionalPanel(
            condition = "input.Info_Selection== 'Method'",
            p(h4("Method")),
            tags$div(
              #strong(style = "font-size: 18px;","Description"),
             # tags$li("Write the method")
            ),
            tags$div(
              style = "display: flex; flex-direction: column; align-items: flex-start;",
              tags$strong(style = "font-size: 18px;", "Equations"),
              tags$div(
                style = "font-size: 14px; margin-top: 10px;",
                withMathJax(HTML(equations_text))

              )
            )
          ),
          conditionalPanel(
            condition = "input.Info_Selection == 'Instructions'",
            p(h4("Instructions")),
            radioButtons(
              "Tabs_Info",
              "Tabs Info",
              choices = list(
                "Parameters & Uncertainty" = "Parameters_Uncertainty",
                "Plot settings" = "Plot_settings",
                "Advanced settings" = "Advanced_settings"
              ),
              inline = T,
              selected = "Parameters_Uncertainty"),
              
          conditionalPanel(
            condition = "input.Tabs_Info == 'Parameters_Uncertainty'",
            HTML(info_param_uncert_tab_html)
          ),
          conditionalPanel(
            condition = "input.Tabs_Info == 'Advanced_settings'",
            HTML(info_advance_tab)
        
          ),
        ),
        conditionalPanel(
          condition = "input.Info_Selection == 'Authors_list'",
          p(h4("Authors")),
          p(h5(HTML("Yumi Cleary<sup>1,2</sup>, Nicolo Milani<sup>1</sup>, Kayode Ogungbenro<sup>2</sup>, Leon Aarons<sup>2</sup>, Aleksandra Galetin<sup>2</sup>, Michael Gertz<sup>1</sup>"))),
          p(h4("Affiliation")),
          p(h5(HTML("<sup>1</sup>Roche Pharma Research and Early Development, Pharmaceutical Sciences, Roche Innovation Center Basel, Switzerland"))),
          p(h5(HTML("<sup>2</sup>Centre for Applied Pharmacokinetic Research, Division of Pharmacy and Optometry, School of Health Sciences, University of Manchester, Manchester, UK"))),
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(
            3,  # Full width for the table
            tags$head(
              tags$style(HTML("
                        .dataTables_wrapper .dataTables_length,
                        .dataTables_wrapper .dataTables_filter,
                        .dataTables_wrapper .dataTables_info,
                        .dataTables_wrapper .dataTables_paginate {
                          font-size: 16px;
                        }
                        table.dataTable tbody th, table.dataTable tbody td {
                          font-size: 16px;
                        }
                        table.dataTable thead th {
                          font-size: 18px;
                        }
                        #table-container {
                          margin-left: +2cm;
                          margin-top: 1cm;
                        }
                      ")
              )
            ),
            tags$head(
              tags$style(HTML("
                        .move-right {
                          margin-left: 200px; 
                        }
                      "))
            ),
            #tags$h4("Estimation and Uncertainty"),
            div(id = "table-container", DTOutput("table"),
                #7uiOutput("number")
            )
          )
        ),
        fluidRow(
          column(
            12,  # Full width for the plot
            plotOutput("fm_fg_plot", height = "700px", width = "700px")
          )
        ),
        uiOutput("message")
      )
    )
  )
}



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    values <- reactiveValues()
  
    output$fm_fg_plot <- renderPlot({
      
      Observations = input$Observations
      Points = input$Points
      
      # reset parameters setting for the drug and ddi param
      if (input$CLp_var != 0.1| input$Rb_var != 0.1 | input$Vlk_var != 0.1 | input$CLR_var != 0.1  ) {
        shinyjs::enable("activateButton_drug_param")  # Enable the button
      } else {
        shinyjs::disable("activateButton_drug_param")  # Disable the button
      }
    
      if (input$AUCR_var != 0.1 | input$CMXR_var != 0.1   | input$tmaxPO_var != 0.1  ) {
        shinyjs::enable("activateButton_ddi_param")  # Enable the button
      } else {
        shinyjs::disable("activateButton_ddi_param")  # Disable the button
      }
      tWT_M           <-input$WT_M # assuming 70 kg as body weight (range:52-85kg as reported in Olkkola 1994) 
      tWT_F           <-input$WT_F # assuming 70 kg as body weight (range:52-85kg as reported in Olkkola 1994) 
      
      tCLp          <-input$CLp # mL/min/kg 
      tRb           <-input$Rb
     
      tVlk          <-input$Vlk
      tQh           <- 97 # Yang and Jamei (2007) for 70 kg adults
      tCLb           <-(tCLp)/tRb # and convert CLp in blood CL in L/h
      
      #tfup = 1
      #fub = tfup/input$Rb
      
      ###   extra hepatic CL
      tCLR =  input$CLR 
      tCLRB      <-tCLR/tRb
      
      # Hepatic CL
      tCLH= tCLb-tCLRB
      tCLHint = tCLH/((1-tCLH/tQh))      # intrinsic CLHb # intrinsic CLH
    
      
      # Additional parameters
      tVb           <-tVlk/tRb          # assuming 70 kg as body weight
      tke          <-tCLb/tVb
      t.tmax.po     <- input$tmax.po 
      t.f.tmax      <-function(p){log(p/tke)/(p-tke)}
      tka          <-uniroot(function(p) t.f.tmax(p)-t.tmax.po, c(0.0001,10))$root
      
      tAUCR        <-input$AUCR # AUC ratio
      tCMXR        <-input$CMXR  # Cmax ratio

      tinh   <- (100-input$inh )/100
      tInhGI = (100-input$inhGI )/100
      
      print(tinh)
      values$CLHint <- tCLHint
      values$tQh <- tQh
      values$Vb <- tVb
      values$ka <- tka
      values$tCMXR <- tCMXR
      values$tAUCR <- tAUCR
      values$inh <- tinh
      values$fmet <- tCLH/tCLb
      
      
      ### Calculate the most likely fm and Fg
      l.fm    <-seq(0.001,0.999,Points) 
      
      # est.fm_50=1
      tryCatch({
        est.fm_50   <-uniroot(function(l.fm) f.r1(fm=l.fm,tCLHint,tAUCR,tinh,tQh,tCLR,tInhGI,tRb)-f.r2(fm=l.fm,tQh,tCLHint,tinh,tVb,tka,tCMXR,tCLR,tInhGI ,tRb), c(-0.0001,0.999))$root 
        est.Fg_50   <-f.r1(est.fm_50,tCLHint,tAUCR,tinh,tQh,tCLR,tInhGI,tRb)
        print("FG")
        print(est.Fg_50)
        print("fm")
        print(est.fm_50)
         
        df_tv <- data.frame(
          "x" = l.fm,  # numeric column
          "C" =  f.r2(fm=l.fm,tQh,tCLHint,tinh,tVb,tka,tCMXR,tCLR,tInhGI,tRb) , # character column
          "A" = f.r1(fm=l.fm,tCLHint,tAUCR,tinh,tQh, tCLR,tInhGI,tRb)  # character column
        )

        # Your code to process the data goes here
      }, error = function(e) {
        # Executed if there is an error
        est.fm_50 <<- "out of range"
        est.Fg_50   <<- NA
        
      })
      # Define the function to solve for fm
      # solve_fm <- function(AUCR, CLintH, Qh, CLNH, inhH) {
      #   # Define the equation as a function of fm
      #   equation <- function(fm) {
      #     numerator <- (CLintH * (Qh + CLNH) + CLNH * Qh)
      #     denominator <- ((fm * CLintH * inhH + (1 - fm) * CLintH) * (Qh + CLNH) + CLNH * Qh)
      #     AUCR - (numerator / denominator)
      #   }
        
        # Use uniroot to find the root of the equation
       # result <- uniroot(equation, c(0, 1))
       # return(result$root)
      #}
     # fm_from_AUC <-  solve_fm(tAUCR, tCLHint, tQh, tCLRB, tinh)
      
      if (input$set_seed==TRUE){
        set.seed(123)
      }
       print("fm max new")
      #print(fm_from_AUC)
      ########### Evaluate the uncertainty of the predcited fm and FG
      
      ## **Mathematical 2D analysis with assuming 95% inhibition**
      etaClp <- rnorm(Observations, mean = 0, sd = input$CLp_var)
      etaCl_R <- rnorm(Observations, mean = 0, sd = input$CLR_var)
      etaRb <- rnorm(Observations, mean = 0, sd = input$Rb_var)
      etaVlk <- rnorm(Observations, mean = 0, sd = input$Vlk_var)
      #etaWT <- rnorm(Observations, mean = 0, sd = input$WT_var)
      etaAUCR <- rnorm(Observations, mean = 0, sd = input$AUCR_var)
      etaCMXR <- rnorm(Observations, mean = 0, sd = input$CMXR_var)
      etatmax.po <- rnorm(Observations, mean = 0, sd = input$tmaxPO_var)
      eta_Height <- rnorm(Observations, mean = 0, sd = input$Height_var)
      eta_Age <- rnorm(Observations, mean = 0, sd = input$Age_var)
      
      Sex_ratio = input$Sex_ratio   # selected by the user?
      N_female= round(input$Observations*Sex_ratio)
      N_male= input$Observations-N_female
      
    
      Sex_Pop = sample(c(0, 1), input$Observations, replace = TRUE)
       
      WT_TVmale = tWT_M
      WT_TVfemale = input$WT_F
      
      SIGMA_WTmale = 0.0365  
      SIGMA_WTfemale= 0.0365
      ETA_WT <- qnorm(randomLHS(input$Observations,k=1), mean = 0, sd = SIGMA_WTmale)*(1-Sex_Pop) + qnorm(randomLHS(input$Observations,k=1), mean = 0, sd = SIGMA_WTmale)*(Sex_Pop)
      WT_Pop = WT_TVmale*exp(ETA_WT)*(1-Sex_Pop) + WT_TVfemale*exp(ETA_WT)*Sex_Pop 
      
      
      #print("eta")
      #print((WT_Pop))
      
      df <- generate_data_pop(Observations, tCLp, etaClp, tCLR, etaCl_R, tRb, etaRb,
                          tVlk, etaVlk, WT_Pop, ETA_WT, input, eta_Height, 
                          eta_Age, Sex_ratio, tAUCR, etaAUCR, tCMXR, etaCMXR, 
                          t.tmax.po, etatmax.po, l.fm, f.r2, f.r1, tinh, tQh, tInhGI)
      
      df_save<<-df
    
      p <- c(0.05, 0.5, 0.95);
      data <-df %>% group_by(x) %>%
        do(data.frame(p=p,
                      A=quantile(.$A, probs=p, na.rm=T),
                      A.n = length(.$A), A.avg = mean(.$A) ,
                      C=quantile(.$C, probs=p, na.rm=T),
                      C.n = length(.$C), C.avg = mean(.$C))) %>%
        
        mutate(Percentile=factor(sprintf("%d%%",p*100),levels=c("5%","50%","95%")))
      

      # Separate data for 5%, 50%, and 95% percentiles
      data_5 <- data[data$Percentile == '5%',]
      data_50 <- data[data$Percentile == '50%',]
      data_95 <- data[data$Percentile == '95%',]
      
      # Interpolate ymin and ymax to match the length of x
      ymin <- approx(data_5$x, data_5$C, data$x)$y
      ymax <- approx(data_95$x, data_95$C, data$x)$y
      yminA <- approx(data_5$x, data_5$A, data$x)$y
      ymaxA <- approx(data_95$x, data_95$A, data$x)$y
      #ymaxA<<-ymaxA
      #print(ymaxA)
      ##################################
      # x_1 <- seq(0, 1, length.out = 100)
      # y_1 <- rep(1, length(x_1))
      # 
      # # Combine x and y into a data frame
      # data_ <- data.frame(x_1, y_1)
      # 
      # # Print the data frame
      # print(data_)
      # 
      # # Plot the horizontal line
      # plot(data_$x_1, data_$y_1, type = "l", col = "blue", lwd = 2, ylim = c(0, 2),
      #      xlab = "x", ylab = "y", main = "Horizontal Line with Intercept of 1")
      # 
      # ########################################
      # Define the x and y coordinates of the curves
      x1 <- l.fm
      y1 <- data_95$C
      y3 <- data_5$C
      y1A <- data_95$A
      y3A <- data_5$A
      x2 <- l.fm
      y2 <- data_50$A
      
      # Define the functions for the curves
      f1 <- approxfun(x1, y1)
      f3 <- approxfun(x1, y3)
      f2 <- approxfun(x2, y2)
      f1A <- approxfun(x1, y1A)
      f3A <- approxfun(x1, y3A)
      
      f95AC2 <- function(x) f1(x) - f3A(x)
      f5AC2 <- function(x) f3(x) - f1A(x)
      
      # FG_lim_max <- NULL
      # # Use tryCatch to handle potential errors
      # result95AC2 <- tryCatch({
      #   uniroot(f95AC2, range(x1))
      # }, error = function(e) {
      #   NULL
      # })
      # 
      # if (!is.null(result95AC2)) {
      #   values$fm_max <- result95AC2$root
      #   FG_lim_max <- f1(result95AC2$root)
      # } else {
      #   values$fm_max <- "1"
      #   FG_lim_max <- "1"
      # }
      # print("RRRRRRRRRRRRRR")
      # print(FG_lim_max)
      # 
      # 
      # 
      # # Initialize FG_lim_min and values$fm_min
      # FG_lim_min <- NULL
      # 
      # # Use tryCatch to handle potential errors
      # result5AC2 <- tryCatch({
      #   uniroot(f5AC2, range(x1))  # Adjust the function and range as needed
      # }, error = function(e) {
      #   NULL
      # })
      # 
      # if (!is.null(result5AC2)) {
      #   values$fm_min <- result5AC2$root
      #   FG_lim_min <- f3(result5AC2$root)
      # } else {
      #   values$fm_min <- "0"
      #   FG_lim_min <- "0"
      # }
      # 
      # # Print the results (optional)
      # print(values$fm_min)
      # print(FG_lim_min)
      
      # Attempting to calculate the upper lim
      lim_fm_max=1
      tryCatch({
        result95AC2 <- uniroot(f95AC2, range(x1)) # intersect ymaxC and yminA
        lim_fm_max= result95AC2$root
        values$fm_max <- (lim_fm_max)
        FG_lim_max = (f1(lim_fm_max))     # intersect ymaxA and ymaxC
        # Your code to process the data goes here
      }, error = function(e) {
        # Executed if there is an error
        lim_fm_max <<- "1"
        FG_lim_max <<- '1'
        values$fm_max <- lim_fm_max
        
      })
      
      
      
      
      # Attempting to calculate the lower lim
      lim_fm_min = 0
      tryCatch({
        result5AC2 <- uniroot(f5AC2, range(x1))   # intersect ymaxA and yminC
        print(result5AC2)
        lim_fm_min = result5AC2$root
        print("fm min")
        print(lim_fm_min)
        values$fm_min <- (lim_fm_min)
        FG_lim_min = (f3(lim_fm_min))   # intersect yminA and yminC
        print(FG_lim_min)
        # Your code to process the data goes here
      }, error = function(e) {
        # Executed if there is an error
        print("fm min error")
        lim_fm_min <<- "0"
        FG_lim_min <<- '0'
        values$fm_min <- lim_fm_min
        
      })
      
      
      
      
      if (est.fm_50 == 'out of range') {
        lim_fm_max <- NA
        lim_fm_min <- NA
      }
      if (FG_lim_max != '1' | FG_lim_min != '0') {
        data_5_fm_overlap <- data[data$A > lim_fm_min & data$C > FG_lim_min,]
        data_fm_overlap <- data_5_fm_overlap[data_5_fm_overlap$A < lim_fm_max & data_5_fm_overlap$C < FG_lim_max,]
        correlation_fm_fg <- cor(data_fm_overlap$A, data_fm_overlap$C)
      } else {
        correlation_fm_fg <- NA
      }

      values$correlation_fm_fg <- correlation_fm_fg
      
      FG_lim_min <- ifelse(FG_lim_min < 0 | is.na(FG_lim_min), '0', FG_lim_min)
      FG_lim_max <- ifelse(FG_lim_max > 1 | is.na(FG_lim_max), '1', FG_lim_max)
      
      values$fG_min <- ifelse(FG_lim_min < 0, '0', FG_lim_min)
      values$fG_max <- ifelse(FG_lim_max < 0, '1', FG_lim_max)
      values$fG_min <- ifelse(FG_lim_min > 1, '1', FG_lim_min)
      values$est.Fg_50 <- ifelse(est.Fg_50 > 1, 1, est.Fg_50)
      values$est.fm_50 <- est.fm_50
      
      if (values$est.fm_50 != 'out of range') {
        values$lim_20perc_up_fm <- values$est.fm_50 * 1.2
        values$lim_20perc_up_FG <- values$est.Fg_50 * 1.2
        values$lim_20perc_down_fm <- values$est.fm_50 * 0.8
        values$lim_20perc_down_FG <- values$est.Fg_50 * 0.8
        values$lim_50perc_up_fm <- values$est.fm_50 * 1.5
        values$lim_50perc_up_FG <- values$est.Fg_50 * 1.5
        values$lim_50perc_down_fm <- values$est.fm_50 * 0.5
        values$lim_50perc_down_FG <- values$est.Fg_50 * 0.5
      }
      
      # Check if the necessary data and conditions exist, this avoid to show the plot in the UI with the error message
      validate(
        need(exists("data"), ""),
        need(exists("df_tv"), ""),
        need(exists("ymin"), ""),
        need(exists("ymax"), ""),
        need(exists("yminA"), ""),
        need(exists("ymaxA"), ""),
        need(exists("est.fm_50"), ""),
        need(exists("est.Fg_50"), ""),
        need(exists("lim_fm_min"), ""),
        need(exists("FG_lim_min"), ""),
        need(exists("lim_fm_max"), ""),
        need(exists("FG_lim_max"), ""),
        need(!is.null(input$Show_seg_var), "")
      )
      
      # Create the plot
      p <- create_plot(data, df_tv, ymin, ymax, yminA, ymaxA, est.fm_50, est.Fg_50, lim_fm_min, FG_lim_min, lim_fm_max, FG_lim_max, input$Show_seg_var)
      plot(p)
      print(values$fG_min)
  
    })
   #values <- reactiveValues()
    
    
    output$table <- renderDT({
      print(values$lim_20perc_down_FG)
      
      values$fmet.cyp3a <-NA
      values$fmet.cyp3a_min <-NA
      values$fmet.cyp3a_max <-NA
      values$message_suggestion=""
      if (values$est.fm_50!='out of range'){
        values$fmet.cyp3a <-values$fmet*values$est.fm_50
        
        print("fmetcyp3a4")
        print(values$fmet.cyp3a)
        if (values$fm_min > values$lim_20perc_down_fm  && values$fm_max < values$lim_20perc_up_fm ) {
          emojiCode_fm  <- "\U1F7E2"  # Green circle emoji (U+1F7E2)
          color <- "green"
          
        } else if (values$fm_min < values$lim_50perc_down_fm  || values$fm_max > values$lim_50perc_up_fm  ){
          emojiCode_fm <- "\U1F534"  # Red circle emoji (U+1F534)
          color <- "red"
          
        }else{
          emojiCode_fm <- "\U1F7E1"  # Orange circle emoji (U+1F7E1)
          color <- "orange"
        }
        
        if (values$fm_min!='0'){
          values$fmet.cyp3a_min <-values$fmet*values$fm_min
          
        }
        if (values$fm_max!='1'){
          values$fmet.cyp3a_max <-values$fmet*values$fm_max
          
        } else if (values$fm_max=='1'){
          values$fmet.cyp3a_max <-"1"
        }
        
        print(values$lim_20perc_down_FG)
        
      } else{
        emojiCode_fm  <- ""  # Black circle emoji (U+1F534)
        color <- "black"
        values$fmet.cyp3a <-NA
        values$fmet.cyp3a_min <-NA
        values$fmet.cyp3a_max <-NA
      }
      
      if (values$est.fm_50!='out of range'){
        if ( values$fG_min =="1"  && values$fG_max =="1" && values$est.Fg_50 ==1  ){
          emojiCode_fm <- "\U1F7E1"  # Red circle emoji (U+1F534)
          emojiCode_FG <- "\U1F7E1"  # Red circle emoji (U+1F534)
          emojiCode_fm <- "\U1F7E1"  # Red circle emoji (U+1F534)
          color <- "orange"
          values$message_suggestion ="fg=0"
        } else if ( values$est.fm_50 <0.01){
          emojiCode_fm <- "\U1F7E1"  # Red circle emoji (U+1F534)
          emojiCode_FG <- "\U1F7E1"  # Red circle emoji (U+1F534)
          emojiCode_fm <- "\U1F7E1"  # Red circle emoji (U+1F534)
          color <- "orange"
          values$message_suggestion ="fm=0"
        } else if ( values$fG_min > values$lim_20perc_down_FG  &&  values$fG_max  < values$lim_20perc_up_FG ) {
          emojiCode_FG <- "\U1F7E2"  # Green circle emoji (U+1F7E2)
          color <- "green"
        } else if ( values$fG_min < values$lim_50perc_down_FG  || values$fG_max > values$lim_50perc_up_FG  ){
          emojiCode_FG <- "\U1F534"  # Red circle emoji (U+1F534)
          color <- "red"
          
        }else{
          emojiCode_FG <- "\U1F7E1"  # Orange circle emoji (U+1F7E1)
          color <- "orange"
        }
      } else{
        emojiCode_FG <- ""  # Black circle emoji (U+1F534)
        color <- "black"
      }
      
      
      print(values$fG_max)
      
      fm_min = round_to_three_digits(values$fm_min)
      fm_max = round_to_three_digits(values$fm_max)
      fmet.cyp3a_min =round_to_three_digits(values$fmet.cyp3a_min)
      fmet.cyp3a_max = round_to_three_digits(values$fmet.cyp3a_max)
      
      df <- data.frame(
        Parameter = c("fm[enzyme]", "FG", "fm[overall]"),
        Estimate = c(values$est.fm_50, values$est.Fg_50, values$fmet.cyp3a),
        Lower = c(fm_min, values$fG_min, fmet.cyp3a_min),
        Upper = c(fm_max, values$fG_max,fmet.cyp3a_max),
        Confidence = c(emojiCode_fm,emojiCode_FG, emojiCode_fm),
        stringsAsFactors = FALSE
      )
      
      # Convert numeric columns to character only if they are numeric
      if (is.numeric(df$Estimate)) {
        df$Estimate <- formatC(df$Estimate, format = "f", digits = 3)
      }
      if (is.numeric(df$Lower)) {
        df$Lower <- formatC(df$Lower, format = "f", digits = 3)
      }
      if (is.numeric(df$Upper)) {
        df$Upper <- formatC(df$Upper, format = "f", digits = 3)
      }
      
      # print df in your Shiny app
      
      datatable(df, options = list(
        dom = 't',  # Only show the table (no search box, pagination, etc.)
        autoWidth = FALSE,
        columnDefs = list(list(width = '200px', targets = "_all"))      
      ))
      
      
    })
    
    
    output$plot2 <- renderPlot({
      # Get your x and y values here
      x <- values$est.fm_50
      y <- values$est.Fg_50 
      CLHint <- values$CLHint
      tQh = values$tQh 
      fub = values$fub
      Vb = values$Vb
      ka = values$ka 
      tCMXR = values$tCMXR
      tAUCR = values$tAUCR  
      inh = values$inh
      print(x)
      print(y)
      print(CLHint)
      print(tQh)
      print(fub)
      print(Vb)
      print(ka)
      
      print(tCMXR)
      
      print(tAUCR)
      print(tAUCR)
      
      
      # Call function toget tangent, it is not working (necessary?)
      #perform_fm_fg_identifiability(x,y, tQh, CLHint, fub, Vb,ka, tCMXR,tAUCR,inh)
    })
    
    observeEvent(input$activateButton_drug_param, {
      updateNumericInput(session, "CLp_var", value = 0.1)
      updateNumericInput(session, "Rb_var", value = 0.1)
      updateNumericInput(session, "Vlk_var", value = 0.1)
      updateNumericInput(session, "CLR_var", value = 0.1)
      
      shinyjs::disable("activateButton_drug_param")  # Disable the button after clicking
    })
    
    observeEvent(input$activateButton_ddi_param, {
      updateNumericInput(session, "AUCR_var", value = 0.1)
      updateNumericInput(session, "CMXR_var", value = 0.1)
      #updateNumericInput(session, "WT_var", value = 0.1)
      updateNumericInput(session, "tmaxPO_var", value = 0.1)
      
      
      shinyjs::disable("activateButton_ddi_param")  # Disable the button after clicking
    })
    
    
    
    output$number <- renderUI({
      correlation_fm_fg <- values$correlation_fm_fg
      HTML(paste0("<span style='font-size: 20px;'>Correlation= ", round(correlation_fm_fg, 3), "</span>"))
    })
    
    
    
    
    output$message <- renderUI({
      if (values$est.fm_50 == "out of range") {
        tags$h2(style = "color: red; font-family: Arial, sans-serif;", "No intersection found, potentially not a purely metabolic DDI")
      } else if (values$message_suggestion=="fg=0"){
        tags$h2(
          style = "color: orange; font-family: Arial, sans-serif;", 
          HTML("Suggestion:<br> The FG estimated from the input <br> parameters suggests no intestinal metabolism! <br> Please check the input parameters")
        )
      } else if (values$message_suggestion=="fm=0"){
        tags$h2(
          style = "color: orange; font-family: Arial, sans-serif;", 
          HTML("Suggestion:<br> The fm estimated from the input <br> parameters suggests no hepatic metabolism! <br> Please check the input parameters")
        )
      } else{
        NULL
      }
    })
    
    # Conditionally render plot and table based on selected tab
    observe({
      if (input$main_navbar != "Model Info") {
        shinyjs::show("fm_fg_plot")
        shinyjs::show("table")
        shinyjs::show("number")
        shinyjs::show("header")
        
        
      } else {
        shinyjs::hide("fm_fg_plot")
        shinyjs::hide("table")
        shinyjs::hide("number")
        shinyjs::hide("header")
        
        
      }
    })
  ############ AUCR
    # Flags to control updates
    updating_AUCR_int <- FALSE
    updating_AUCR_var <- FALSE
    updating_AUCR <- FALSE
    
    # Reactive expression to calculate confidence interval for AUCR
    AUC_int_calc <- reactive({
      req(input$AUCR_var)
      calculate_confidence_interval(input$AUCR, input$AUCR_var)
    })
    
    # Reactive expression to calculate sigma for AUCR
    sigma_calc_AUC <- reactive({
      req(input$AUCR_int)
      calculate_sigma(input$AUCR, input$AUCR_int)
    })
    
    # Observer to update AUCR_int based on AUCR_var
    observeEvent(input$AUCR_var, {
      if (!updating_AUCR_int && !updating_AUCR) {
        updating_AUCR_var <<- TRUE
        new_CI <- AUC_int_calc()
        print("int updated")
        print(new_CI)
        updateTextInput(session, "AUCR_int", value = paste(new_CI[1], new_CI[2], sep = ", "))
        updating_AUCR_var <<- FALSE
      } 
    })
    
    # Observer to update AUCR_var based on AUCR_int when the button is clicked
    observeEvent(input$confirm_AUCR_int, {
      if (!updating_AUCR_var && !updating_AUCR) {
        updating_AUCR_int <<- TRUE
        new_sigma <- round(sigma_calc_AUC()[1], 2)  # Round to 2 decimal places
        updateNumericInput(session, "AUCR_var", value = new_sigma)
        if (sigma_calc_AUC()[4] == 0) {
          message_sigma_AUCR <- "<br>The CIs provided are not log<br>distributed for the typical value, check the inputs"
        } else {
          message_sigma_AUCR <- ""
        }
        print("message")
        print(message_sigma_AUCR)
        output$message_sigma_AUCR <- renderText({ message_sigma_AUCR })  # Update the message output
        updating_AUCR_int <<- FALSE
      }
    })
    
    # Observer to update AUCR_int and AUCR_var based on AUCR
    observeEvent(input$AUCR, {
      if (!updating_AUCR_var && !updating_AUCR_int) {
        updating_AUCR <<- TRUE
        new_sigma <- round(sigma_calc_AUC()[1], 2)  # Round to 2 decimal places
        new_CI <- AUC_int_calc()
        updateNumericInput(session, "AUCR_var", value = new_sigma)
        updateTextInput(session, "AUCR_int", value = paste(new_CI[1], new_CI[2], sep = ", "))
        updating_AUCR <<- FALSE
      }
    })
  
  ########## CMAXR
  # Flags to control updates
  updating_CMXR_int <- FALSE
  updating_CMXR_var <- FALSE
  updating_CMXR <- FALSE
  
  # Reactive expression to calculate confidence interval for AUCR
  CMXR_int_calc <- reactive({
    req(input$CMXR_var)
    calculate_confidence_interval(input$CMXR, input$CMXR_var)
  })
  
  # Reactive expression to calculate sigma for AUCR
  sigma_calc_CMX <- reactive({
    req(input$CMXR_int)
    calculate_sigma(input$CMXR, input$CMXR_int)
  })
  
  # Observer to update CMXR_int based on CMXR_var
  observeEvent(input$CMXR_var, {
    if (!updating_CMXR_int && !updating_CMXR) {
      updating_CMXR_var <<- TRUE
      new_CI <- CMXR_int_calc()
      print("int updated")
      print(new_CI)
      updateTextInput(session, "CMXR_int", value = paste(new_CI[1], new_CI[2], sep = ", "))
      updating_CMXR_var <<- FALSE
    } 
  })
  
  # Observer to update CMXR_var based on CMXR_int when the button is clicked
  observeEvent(input$confirm_CMXR_int, {
    if (!updating_CMXR_var && !updating_CMXR) {
      updating_CMXR_int <<- TRUE
      new_sigma <- round(sigma_calc_CMX()[1], 2)  # Round to 2 decimal places
      updateNumericInput(session, "CMXR_var", value = new_sigma)
      if (sigma_calc_CMX()[4]==0){
        message_sigma_CMXR <- "<br>The CIs provided are not log<br>distributed for the typical value, check the inputs"
      } else {
        message_sigma_CMXR= ""
      }
      print("message")
      print(message_sigma_CMXR)
      output$message_sigma_CMXR <- renderText({ message_sigma_CMXR })  # Update the message output
      updating_CMXR_int <<- FALSE
    }
  })
  
  # Observer to update AUCR_int and AUCR_var based on AUCR
  observeEvent(input$CMXR, {
    if (!updating_CMXR_var && !updating_CMXR_int) {
      updating_CMXR <<- TRUE
      new_sigma <- round(sigma_calc_CMX()[1], 2)  # Round to 2 decimal places
      new_CI <- CMXR_int_calc()
      updateNumericInput(session, "CMXR_var", value = new_sigma)
      updateTextInput(session, "CMXR_int", value = paste(new_CI[1], new_CI[2], sep = ", "))
      updating_CMXR <<- FALSE
    }
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)


