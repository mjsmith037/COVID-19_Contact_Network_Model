#' RShiny Interface for plotting network simulation results
#' June 19, 2020
#' @author Lauren White, lwhite@sesync.org
#' @author Matthew J. Michalksa-Smith mjsmith037@gmail.com
#' adapted from: https://tpetzoldt.github.io/deSolve-shiny/deSolve-shiny.html

source("common_parameters.R")
source("common_functions.R")
source("quantify_disease_outcomes.R")

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyalert)
library(kableExtra)
library(shinyjs)
library(jsonlite)

# library(googlesheets4)
# gs4_auth(cache=".secrets", email="michalsm@umn.edu")

library(igraph, warn.conflicts=FALSE)
library(tidygraph, warn.conflicts=FALSE)

library(ggraph)
library(patchwork)

library(Rcpp)

steps <- read_csv("www/tutorial.csv", col_types="nccc")

#####
#' NOTE: for every doubling of number of households, expect an approximate
#' sextupling of time needed to run. Relatively independent of MAXTIME, as
#' simulation stops at steady state anyway. Default parameters with 1000
#' households take ~2 second to run. There is some dependence on the disease
#' parameters, as things like the duration of the epidemic and the number of
#' infectious individuals affect the number of loops needed.
#####
sourceCpp("simulate_disease_on_network.cpp")

source("generate_contact_network.R")

write_data <- function(input, simulation_output, source_tab) {
  values <- isolate(reactiveValuesToList(input)) %>%
    .[str_detect(names(.), "info|recalc|main|_1|_2|shinyalert|_0$", negate=TRUE)] %>%
    .[order(names(.))]
  if (source_tab == "scenario") {
    # fill parameters that are irrelevant to scenario with NAs
    values <- within(values, rm("sandbox", "scenario_params"))
    if (!("1. Adding Household-Merging" %in% values$scenario)) {
      values$between_household_transmission_rate <- NA
      values$number_households_merged <- NA
    } else if (values$scenario == "1. Adding Household-Merging") {
      values$classmate_transmission_rate <- NA
      values$approx_classroom_size <- NA
      values$coworker_transmission_rate <- NA
    } else if (values$scenario == "2. Household-Merging vs. School") {
      values$coworker_transmission_rate <- NA
    } else if (values$scenario == "3. School vs. Work") {
      values$between_household_transmission_rate <- NA
      values$number_households_merged <- NA
    } else warning("unexpected value for 'scenario'")
    values$background_transmission_rate <- get_background(values$background_transmission_rate)
    values %>%
      as_tibble() %>%
      bind_cols(summarize_epidemic(simulation_output)) %>%
      sheet_append(ss="https://docs.google.com/spreadsheets/d/1RS6-7cddLEAsdSpwCQ-UOYEggwRxVAlG8uVV2tafBQ0/edit#gid=0",
                   sheet="output_v2_scenarios")
  } else if (source_tab == "sandbox") {
    # fill unused parameters with NAs
    values <- within(values, rm("scenario", "scenario_params"))
    if (!any(values$sandbox == "Household-Merging")) {
      values$between_household_transmission_rate <- NA
      values$number_households_merged <- NA
    } else if (!any(values$sandbox == "In-person Schooling")) {
      values$classmate_transmission_rate <- NA
      values$approx_classroom_size <- NA
    } else if (!any(values$sandbox == "In-person Work")) {
      values$coworker_transmission_rate <- NA
    }
    values$background_transmission_rate <- get_background(values$background_transmission_rate)
    values$sandbox <- str_c(values$sandbox, collapse=";")
    values %>%
      as_tibble() %>%
      bind_cols(summarize_epidemic(simulation_output)) %>%
      sheet_append(ss="https://docs.google.com/spreadsheets/d/1RS6-7cddLEAsdSpwCQ-UOYEggwRxVAlG8uVV2tafBQ0/edit#gid=0",
                   sheet="output_v2_sandbox")
  }
}

ui <- fluidPage(
  tags$head(
    # tags$link(rel = "shortcut icon", type = "image/png", href = "image.png"),
    tags$title("COVID Network Model")
  ),
  withMathJax(),
  includeCSS("www/main.css"),
  includeCSS("www/introjs.min.css"),
  includeScript("www/intro.min.js"),
  includeScript("www/app.js"),
  navbarPage(
    theme = bs_theme(bootswatch="yeti"),
    title=div("COVID Network Model", tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right;\"><img class=\"logoimg\" src=\"UMN_logo_M.png\"><img class=\"logoimg biglogo\" src=\"UMN_logo_rest.png\"></div>');
header.prepend('<div style=\"float:left; padding-right: 24px;\"><a href=\"https://mcraftlab.wordpress.com/\"><img class=\"logoimg\" src=\"Craft Lab logo puma.jpg\"></a></div>');\
console.log(header)"))),
    id = "main_navbar",
    #### Welcome ####
    tabPanel("Welcome!", div(class="center", markdown(read_file("www/text/welcome.md")))),
    #### Tutorial ####
    tabPanel(id='step1',
             "Tutorial",
             sidebarLayout(
               sidebarPanel(
                 width=3, id='step2',
                 div(id="step3",
                     tooltip(sliderInput(inputId = "transmission_rate", value = 0.5, min = 0, max = 1, step=0.05,
                                         label = "Transmission rate \\(\\beta\\)"),
                             "The liklihood of an interaction between an infectious individual and susceptible individual resulting in infection",
                             placement="right")
                 ),
                 tooltip(sliderInput(inputId="sigma_0", value=3.7, min=3.3, max=4, step=0.1,
                                     label="Length of latent period \\(1/\\sigma\\)"),
                         "Average number of days from exposure to the virus to becoming infectious",
                         placement="right"),
                 sliderInput(inputId="rho_0", value=0.25, min=0.18, max=0.31, step=0.01,
                             label="The proportion of exposed individuals that become symptomatic \\(\\rho\\)"),
                 div(id="step4",
                     tooltip(sliderInput(inputId="gamma_0", value=3.5, min=3.1, max=4, step=0.1,
                                         label="Length of infectious period \\(1/\\gamma\\)"),
                             "Average number of days from becoming infectious to no longer transmitting the virus",
                             placement="right")
                 ),
                 sliderInput(inputId = "mu_0", value = 0.001, min = 0, max = 0.1,
                             label = "Death rate of symptomatic infectious individuals \\(\\mu\\)"),
                 tooltip(sliderInput(inputId = "nu_0", value = 0.14, min = 0, max = 0.33,
                                     label = "Additional death rate for vulnerable individuals \\(\\nu\\)"),
                         "How much more likely are vulnerable people to die from COVID-19 compared to non-vulnerable people?",
                         placement="right"),
                 fluidRow(column(width=6, div(actionButton("info_intro", span("Tutorial", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                          column(width=6, div(id="step5", style="height:4vh;",
                                              actionButton("recalc_intro", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))
                 
               ),
               mainPanel(width=9,
                         fluidRow(id="plots",
                                  column(width=5, div(id="step6", plotOutput("netplot_intro"))),
                                  column(width=7, div(id="step7",
                                                      tabsetPanel(
                                                        tabPanel("Epidemic Curves", plotOutput("lineplot_intro")),
                                                        tabPanel("Proportion of Population", plotOutput("areaplot_intro"))
                                                      )
                                  ))
                         ),
                         fluidRow(id="table")
               )
             )
    ),
    #### Sandbox ####
    tabPanel(
      "Sandbox",
      sidebarLayout(
        sidebarPanel(
          width=3,
          accordion(
            id="sandbox", open="Core Disease Transmission Parameters",
            accordion_panel(
              "Core Disease Transmission Parameters",
              sliderInput(inputId = "number_of_households",
                          label = "Number of households to simulate",
                          value = 100, min = 50, max = 500, step=50),
              tooltip(sliderTextInput(inputId = "background_transmission_rate",
                                      label = "Background transmission rate \\(\\beta_b\\)",
                                      selected = "Medium",
                                      choices = c("Very Low", "Low", "Medium", "High", "Very High")),
                      "General community transmission otherwise unaccounted for",
                      placement="right"),
              tooltip(sliderInput(inputId="sigma", value=3.7, min=3.3, max=4, step=0.1,
                                  label="Length of latent period \\(1/\\sigma\\)"),
                      "Average number of days from exposure to the virus to becoming infectious",
                      placement="right"),
              sliderInput(inputId="rho", value=0.25, min=0.18, max=0.31, step=0.01,
                          label="The proportion of exposed individuals that become symptomatic \\(\\rho\\)"),
              tooltip(sliderInput(inputId="gamma", value=3.5, min=3.1, max=4, step=0.1,
                                  label="Length of infectious period \\(1/\\gamma\\)"),
                      "Average number of days from becoming infectious to no longer transmitting the virus",
                      placement="right"),
              sliderInput(inputId = "mu",
                          label = "Death rate of symptomatic infectious individuals \\(\\mu\\)",
                          value = 0.001, min = 0, max = 0.1),
              tooltip(sliderInput(inputId = "nu",
                                  label = "Additional death rate for vulnerable individuals \\(\\nu\\)",
                                  value = 0.14, min = 0, max = 0.33),
                      "How much more likely are vulnerable people to die from COVID-19 compared to non-vulnerable people?",
                      placement="right")
            ),
            accordion_panel(
              "Household-Merging",
              tooltip(sliderInput(inputId = "between_household_transmission_rate",
                                  label = "Between household transmission rate \\(\\beta_h\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely you are to be infected by members of other households compared to your own",
                      placement="right"),
              tooltip(sliderInput(inputId = "number_households_merged",
                                  label = "Number of other households to join with as a socially monogomous unit",
                                  value = 1, min = 0, max = 10, step=1),
                      "The number of other households that any given household interacts with",
                      placement="right")
            ),
            accordion_panel(
              "In-person Schooling",
              tooltip(sliderInput(inputId = "classmate_transmission_rate",
                                  label = "Classmate transmission rate \\(\\beta_c\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely a child is to be infected by classmates vs by a member of their household",
                      placement="right"),
              tooltip(sliderInput(inputId = "approx_classroom_size",
                                  label = "Approximate size of classrooms",
                                  value = 25, min = 10, max = 40),
                      "for children age 5-19",
                      placement="right")
            ),
            accordion_panel(
              "In-person Work",
              tooltip(sliderInput(inputId = "coworker_transmission_rate",
                                  label = "Coworker transmission rate \\(\\beta_w\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely someone is to be infected by coworkers vs by a member of their household",
                      placement="right")
            )),
          fluidRow(column(width=6, div(actionButton("info_sandbox", span("Show Info", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                   column(width=6, div(actionButton("recalc_sandbox", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))),
        mainPanel(width=9,
                  fluidRow(id="plots",
                           column(width=5, plotOutput("netplot_sandbox"),
                                  div(downloadButton("download_sandbox", span("Download output (", code(".RData", style="background-color:transparent; color:#000000;"), "file)"),
                                                     icon=icon("download"), style="float:right; background-color:#50b99a"))),
                           column(width=7,
                                  tabsetPanel(
                                    tabPanel("Epidemic Curves", plotOutput("lineplot_sandbox")),
                                    tabPanel("Proportion of Population", plotOutput("areaplot_sandbox"))
                                  )
                           )
                  ),
                  fluidRow(id="table")
        )
      )
    ),
    #### Scenarios ####
    tabPanel(
      "Focused Scenarios",
      sidebarLayout(
        sidebarPanel(
          width=3,
          accordion(
            id="scenario_params", open="Core Disease Parameters",
            accordion_panel(
              "Core Disease Parameters",
              sliderInput(inputId = "number_of_households_1",
                          label = "Number of households to simulate",
                          value = 100, min = 50, max = 500, step=50),
              tooltip(sliderTextInput(inputId = "background_transmission_rate_1",
                                      label = "Background transmission rate \\(\\beta_b\\)",
                                      selected = "Medium",
                                      choices = c("Very Low", "Low", "Medium", "High", "Very High")),
                      "General community transmission otherwise unaccounted for",
                      placement="right"),
              tooltip(sliderInput(inputId="sigma_1", value=3.7, min=3.3, max=4, step=0.1,
                                  label="Length of latent period \\(1/\\sigma\\)"),
                      "Average number of days from exposure to the virus to becoming infectious",
                      placement="right"),
              sliderInput(inputId="rho_1", value=0.25, min=0.18, max=0.31, step=0.01,
                          label="The proportion of exposed individuals that become symptomatic \\(\\rho\\)"),
              tooltip(sliderInput(inputId="gamma_1", value=3.5, min=3.1, max=4, step=0.1,
                                  label="Length of infectious period \\(1/\\gamma\\)"),
                      "Average number of days from becoming infectious to no longer transmitting the virus",
                      placement="right"),
              sliderInput(inputId = "mu_1",
                          label = "Death rate of symptomatic infectious individuals \\(\\mu\\)",
                          value = 0.001, min = 0, max = 0.1, step=0.05),
              tooltip(sliderInput(inputId = "nu_1",
                                  label = "Additional death rate for vulnerable individuals \\(\\nu\\)",
                                  value = 0.14, min = 0, max = 0.33),
                      "How much more likely are vulnerable people to die from COVID-19 compared to non-vulnerable people?",
                      placement="right"),
              fluidRow(column(width=6, div(actionButton("info_0", span("Show Scenario Info", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                       column(width=6, div(actionButton("recalc_0", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))
            )
          ),
          accordion(
            id="scenario", open=FALSE, multiple=FALSE,
            accordion_panel(
              "1. Adding Household-Merging",
              tooltip(sliderInput(inputId = "between_household_transmission_rate_1",
                                  label = "Between household transmission rate \\(\\beta_h\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely you are to be infected by members of other households compared to your own",
                      placement="right"),
              tooltip(sliderInput(inputId = "number_households_merged_1",
                                  label = "Number of other households to join with as a socially monogomous unit",
                                  value = 1, min = 0, max = 10, step=1),
                      "The number of other households that any given household interacts with",
                      placement="right"),
              fluidRow(column(width=6, div(actionButton("info_1", span("Show Scenario Info", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                       column(width=6, div(actionButton("recalc_1", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))
            ),
            accordion_panel(
              "2. Household-Merging vs. School",
              tooltip(sliderInput(inputId = "between_household_transmission_rate_2",
                                  label = "Between household transmission rate \\(\\beta_h\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely you are to be infected by members of other households compared to your own",
                      placement="right"),
              tooltip(sliderInput(inputId = "number_households_merged_2",
                                  label = "Number of households to join with into a socially monogomous unit",
                                  value = 1, min = 0, max = 10, step=1),
                      "The number of other households that any given household interacts with",
                      placement="right"),
              tooltip(sliderInput(inputId = "classmate_transmission_rate_1",
                                  label = "Classmate transmission rate \\(\\beta_c\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely a child is to be infected by classmates vs by a member of their household",
                      placement="right"),
              tooltip(sliderInput(inputId = "approx_classroom_size_1",
                                  label = "Approximate size of classrooms",
                                  value = 25, min = 10, max = 40),
                      "for children age 5-19",
                      placement="right"),
              fluidRow(column(width=6, div(actionButton("info_2", span("Show Scenario Info", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                       column(width=6, div(actionButton("recalc_2", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))
            ),
            accordion_panel(
              "3. School vs. Work",
              tooltip(sliderInput(inputId = "classmate_transmission_rate_2",
                                  label = "Classmate transmission rate \\(\\beta_c\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely a child is to be infected by classmates vs by a member of their household",
                      placement="right"),
              tooltip(sliderInput(inputId = "approx_classroom_size_2",
                                  label = "Approximate size of classrooms",
                                  value = 25, min = 10, max = 40),
                      "The approximate size of schools/classrooms for children age 5-19",
                      placement="right"),
              tooltip(sliderInput(inputId = "coworker_transmission_rate_1",
                                  label = "Coworker transmission rate \\(\\beta_w\\)",
                                  value = 0.05, min = 0, max = 1, step=0.05),
                      "How much less likely someone is to be infected by coworkers vs by a member of their household",
                      placement="right"),
              fluidRow(column(width=6, div(actionButton("info_3", span("Show Scenario Info", class="infobtntext"), icon=icon("info-circle"), style="float:left; background-color:#bc4b51"))),
                       column(width=6, div(actionButton("recalc_3", span("Run Simulation", class="runbtntext"), icon=icon("play"), style="float:right; background-color:#7b678e"))))
            ))),
        mainPanel(width=9,
                  fluidRow(id="plots",
                           column(width=5, plotOutput("netplot")),
                           column(width=7,
                                  tabsetPanel(
                                    tabPanel("Epidemic Curves", plotOutput("lineplot")),
                                    tabPanel("Proportion of Population", plotOutput("areaplot"))
                                  )
                           )
                  ),
                  fluidRow(id="table")
        )
      )
    ),
    #### Model Description ####
    tabPanel("Model Description", withMathJax(markdown(read_file("www/text/model_description.md"))))
  )
)

server <- function(input, output, session){
  
  #### update sliders across tabs ####
  observe({updateSliderInput(session, "number_of_households_1", value=input$number_of_households)})
  observe({updateSliderInput(session, "number_of_households", value=input$number_of_households_1)})
  
  observe({updateSliderTextInput(session, "background_transmission_rate_1", selected=input$background_transmission_rate)})
  observe({updateSliderTextInput(session, "background_transmission_rate", selected=input$background_transmission_rate_1)})
  
  observe({updateSliderInput(session, "sigma_0", value=input$sigma)})
  observe({updateSliderInput(session, "sigma", value=input$sigma_0)})
  observe({updateSliderInput(session, "sigma_1", value=input$sigma)})
  observe({updateSliderInput(session, "sigma", value=input$sigma_1)})
  
  observe({updateSliderInput(session, "rho_0", value=input$rho)})
  observe({updateSliderInput(session, "rho", value=input$rho_0)})
  observe({updateSliderInput(session, "rho_1", value=input$rho)})
  observe({updateSliderInput(session, "rho", value=input$rho_1)})
  
  observe({updateSliderInput(session, "gamma_0", value=input$gamma)})
  observe({updateSliderInput(session, "gamma", value=input$gamma_0)})
  observe({updateSliderInput(session, "gamma_1", value=input$gamma)})
  observe({updateSliderInput(session, "gamma", value=input$gamma_1)})
  
  observe({updateSliderInput(session, "mu_0", value=input$mu)})
  observe({updateSliderInput(session, "mu", value=input$mu_0)})
  observe({updateSliderInput(session, "mu_1", value=input$mu)})
  observe({updateSliderInput(session, "mu", value=input$mu_1)})
  
  observe({updateSliderInput(session, "nu_0", value=input$nu)})
  observe({updateSliderInput(session, "nu", value=input$nu_0)})
  observe({updateSliderInput(session, "nu_1", value=input$nu)})
  observe({updateSliderInput(session, "nu", value=input$nu_1)})
  
  #### Tutorial ####
  session$sendCustomMessage(type="setTutorialContent", message=list(steps=toJSON(steps)))
  # only show tutorial on first open of tab
  shown_intro <<- FALSE
  observeEvent(input$main_navbar, ignoreInit=TRUE, {
    if(input$main_navbar == "Tutorial" & !shown_intro) {
      session$sendCustomMessage(type="startTutorial", message=list(""))
      shown_intro <<- TRUE
    }
  })
  # unless button is pressed
  observeEvent(input$info_intro, ignoreInit=TRUE, {
    session$sendCustomMessage(type="startTutorial", message=list(""))
  })
  observeEvent(input$recalc_intro, {
    full_contact_network <- play_smallworld(2, 10, 0, 0.1) %E>% mutate(type="Interaction", weight=input$transmission_rate)
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           rep(FALSE, with_graph(full_contact_network, graph_order())),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           0, 1/input$sigma_0, input$rho_0, 1/input$gamma_0, input$mu_0, input$nu_0)
    output$netplot_intro <- renderPlot(ggraph(full_contact_network, layout=full_contact_network %>% igraph::layout_nicely(weights=. %E>% pull(weight))) +
                                         geom_edge_fan(aes(colour=type), alpha=0.25, show.legend=FALSE) +
                                         geom_node_point(size=3) +
                                         scale_edge_colour_manual(values=my_cols) +
                                         guides(edge_colour=guide_legend(nrow=2, override.aes=list(edge_width=2))) +
                                         # scale_colour_manual(values=my_cols) +
                                         theme_graph(base_family=MAIN_FONT) +
                                         theme(
                                           text=element_text(size=16, family=MAIN_FONT),
                                           plot.title=element_text(face="bold", size=18),
                                           plot.subtitle=element_text(size=12),
                                           plot.caption=element_text(size=12),
                                           legend.title=element_blank(),
                                           legend.text=element_text(size=18),
                                           legend.key.size=unit(18, "pt"),
                                           legend.position="bottom",
                                           legend.margin=margin(l=18, r=18)
                                         ) +
                                         ggtitle("Social Connectivity"))
    output$areaplot_intro <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot_intro <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
  }, ignoreNULL = FALSE)
  
  #### Sandbox ####
  shown_sandbox <<- FALSE
  observeEvent(input$main_navbar, ignoreInit=TRUE, {
    if(input$main_navbar == "Sandbox" & !shown_sandbox) {
      shinyalert("Exploring disease transmission: Welcome to the Sandbox",
                 markdown(read_file("www/text/sandbox.md")),
                 closeOnClickOutside=TRUE, html=TRUE, type="info")
      shown_sandbox <<- TRUE
    }
  })
  observeEvent(input$info_sandbox, ignoreInit=TRUE, {
    shinyalert("Exploring disease transmission: Welcome to the Sandbox",
               markdown(read_file("www/text/sandbox.md")),
               closeOnClickOutside=TRUE, html=TRUE, type="info")
  })
  observeEvent(input$recalc_sandbox, ignoreNULL = FALSE, {
    
    if (!any(input$sandbox == "Household-Merging")) {
      between_household_transmission_rate_tmp <- NULL
      number_households_merged_tmp <- NULL
    } else {
      between_household_transmission_rate_tmp <-
        function(n) rep(input$between_household_transmission_rate *
                          WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n)
      number_households_merged_tmp <- input$number_households_merged + 1
    }
    
    if (!any(input$sandbox == "In-person Schooling")) {
      classmate_transmission_rate_tmp <- NULL
      approx_classroom_size_tmp <- NULL
    } else {
      classmate_transmission_rate_tmp <-
        function(n) rep(input$classmate_transmission_rate *
                          WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n)
      approx_classroom_size_tmp <- input$approx_classroom_size
    }
    
    if (!any(input$sandbox == "In-person Work")) {
      coworker_transmission_rate_tmp <- NULL
    } else {
      coworker_transmission_rate_tmp <-
        function(n) rep(input$coworker_transmission_rate *
                          WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n)
    }
    
    full_contact_network <- generate_contact_network(number_of_households=input$number_of_households,
                                                     # core layer: household transmission
                                                     within_household_transmission_rate=function(n) rep(WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     between_household_transmission_rate=between_household_transmission_rate_tmp,
                                                     number_households_merged=number_households_merged_tmp,
                                                     classmate_transmission_rate=classmate_transmission_rate_tmp,
                                                     approx_classroom_size=approx_classroom_size_tmp,
                                                     coworker_transmission_rate=coworker_transmission_rate_tmp)
    
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           full_contact_network %N>% as_tibble() %>% pull(vulnerable),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           get_background(input$background_transmission_rate),
                                                           1/input$sigma, input$rho, 1/input$gamma, input$mu, input$nu)
    # write_data(input, simulation_output, "sandbox")
    output$netplot_sandbox <- renderPlot(plot_network(full_contact_network))
    output$areaplot_sandbox <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot_sandbox <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
    save(full_contact_network, simulation_output, file="tmp.RData")
  })
  output$download_sandbox <- downloadHandler(filename="simulation_output.RData",
                                             content = function(file) {load("tmp.RData"); save(full_contact_network, simulation_output, file=file)})
  #### Scenarios ####
  # only show info on first open of tab
  shown_0 <<- shown_1 <<- shown_2 <<- shown_3 <<- FALSE
  observeEvent(input$main_navbar, ignoreInit=TRUE, {
    if (input$main_navbar == "Focused Scenarios" & !shown_0) {
      shinyalert("Stay-at-Home Order",
                 markdown(read_file("www/text/scenario_0_description.md")),
                 closeOnClickOutside=TRUE, html=TRUE, type="info")
      shown_0 <<- TRUE
    }
  })
  observeEvent(input$scenario, ignoreInit=FALSE, {
    if (input$scenario == "1. Adding Household-Merging" & !shown_1) {
      shinyalert("Forming combined household units",
                 markdown(read_file("www/text/scenario_1_description.md")),
                 closeOnClickOutside=TRUE, html=TRUE, type = "info")
      shown_1 <<- TRUE
    } else if (input$scenario == "2. Household-Merging vs. School" & !shown_2) {
      shinyalert("Evaluating the effect of opening schools",
                 markdown(read_file("www/text/scenario_2_description.md")),
                 closeOnClickOutside=TRUE, html=TRUE, type = "info")
      shown_2 <<- TRUE
    } else if (input$scenario == "3. School vs. Work" & !shown_3) {
      shinyalert("Comparing school and work transmission",
                 markdown(read_file("www/text/scenario_3_description.md")),
                 closeOnClickOutside=TRUE, html=TRUE, type = "info")
      shown_3 <<- TRUE
    }
  })
  # unless button is pressed
  observeEvent(input$info_0, ignoreInit=TRUE, {
    shinyalert("Stay-at-Home Order",
               markdown(read_file("www/text/scenario_0_description.md")),
               closeOnClickOutside=TRUE, html=TRUE, type="info")
  })
  observeEvent(input$info_1, ignoreInit=TRUE, {
    shinyalert("Forming combined household units",
               markdown(read_file("www/text/scenario_1_description.md")),
               closeOnClickOutside=TRUE, html=TRUE, type = "info")
  })
  observeEvent(input$info_2, ignoreInit=TRUE, {
    shinyalert("Evaluating the effect of opening schools",
               markdown(read_file("www/text/scenario_2_description.md")),
               closeOnClickOutside=TRUE, html=TRUE, type = "info")
  })
  observeEvent(input$info_3, ignoreInit=TRUE, {
    shinyalert("Comparing school and work transmission",
               markdown(read_file("www/text/scenario_3_description.md")),
               closeOnClickOutside=TRUE, html=TRUE, type = "info")
  })
  # also watch button for new simulation
  observeEvent(input$recalc_0, ignoreNULL = FALSE, {
    full_contact_network <- generate_contact_network(number_of_households=input$number_of_households,
                                                     within_household_transmission_rate=function(n) rep(WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n))
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           full_contact_network %N>% as_tibble() %>% pull(vulnerable),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           get_background(input$background_transmission_rate),
                                                           1/input$sigma, input$rho, 1/input$gamma, input$mu, input$nu)
    # write_data(input, simulation_output, "scenarios")
    output$netplot <- renderPlot(plot_network(full_contact_network))
    output$areaplot <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
  })
  observeEvent(input$recalc_1, {
    full_contact_network <- generate_contact_network(number_of_households=input$number_of_households,
                                                     within_household_transmission_rate=function(n) rep(WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     between_household_transmission_rate=function(n) rep(input$between_household_transmission_rate *
                                                                                                           WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     number_households_merged=input$number_households_merged+1)
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           full_contact_network %N>% as_tibble() %>% pull(vulnerable),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           get_background(input$background_transmission_rate),
                                                           1/input$sigma, input$rho, 1/input$gamma, input$mu, input$nu)
    # write_data(input, simulation_output, "scenario")
    output$netplot <- renderPlot(plot_network(full_contact_network))
    output$areaplot <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
  })
  observeEvent(input$recalc_2, {
    full_contact_network <- generate_contact_network(number_of_households=input$number_of_households,
                                                     within_household_transmission_rate=function(n) rep(WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     between_household_transmission_rate=function(n) rep(input$between_household_transmission_rate_2 *
                                                                                                           WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     number_households_merged=input$number_households_merged_2+1,
                                                     classmate_transmission_rate = function(n) rep(input$classmate_transmission_rate *
                                                                                                     WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     approx_classroom_size = input$approx_classroom_size)
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           full_contact_network %N>% as_tibble() %>% pull(vulnerable),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           get_background(input$background_transmission_rate),
                                                           1/input$sigma, input$rho, 1/input$gamma, input$mu, input$nu)
    # write_data(input, simulation_output, "scenario")
    output$netplot <- renderPlot(plot_network(full_contact_network))
    output$areaplot <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
  })
  observeEvent(input$recalc_3, {
    full_contact_network <- generate_contact_network(number_of_households=input$number_of_households,
                                                     within_household_transmission_rate=function(n) rep(WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     classmate_transmission_rate = function(n) rep(input$classmate_transmission_rate_2 *
                                                                                                     WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n),
                                                     approx_classroom_size = input$approx_classroom_size_2,
                                                     coworker_transmission_rate = function(n) rep(input$coworker_transmission_rate *
                                                                                                    WITHIN_HOUSEHOLD_TRANSMISSION_RATE, n))
    simulation_output <- run_disease_simulation_on_network(MAXTIME,
                                                           full_contact_network %E>% select(from, to) %>% as_tibble() %>% as.matrix() %>% subtract(1),
                                                           full_contact_network %>% with_graph(graph_order()) %>% subtract(1) %>% rep(0, .) %>% c(2) %>% sample(),
                                                           full_contact_network %N>% as_tibble() %>% pull(vulnerable),
                                                           full_contact_network %E>% as_tibble() %>% pull(weight),
                                                           get_background(input$background_transmission_rate),
                                                           1/input$sigma, input$rho, 1/input$gamma, input$mu, input$nu)
    
    # write_data(input, simulation_output, "scenario")
    output$netplot <- renderPlot(plot_network(full_contact_network))
    output$areaplot <- renderPlot(plot_area(simulation_output, with_graph(full_contact_network, graph_order())))
    output$lineplot <- renderPlot(plot_line(simulation_output, with_graph(full_contact_network, graph_order())))
  })
}

shinyApp(ui = ui, server = server)
