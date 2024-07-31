#' analyze_grades Shiny App


# 0. Load Packages and Functions ---------------------------------------------


suppressPackageStartupMessages(
  if(!requireNamespace('pacman')) {
    install.packages('pacman', dependencies = TRUE)
  }
)

pacman::p_load(
    shiny,
    DT,
    shinythemes,
    ggplot2,
    dplyr,
    plotly,
    shinyWidgets,
    tibble,
    readr,
    readxl,
    bslib,
    stringr,
    purrr,
    bslib
)


# Source analyze grades function
source('app_files/analyze_grades_function.R')



# 1. UI ----------------------------------------------------------------------


ui <- fluidPage(

  # Set an HTML tag to vertically center the title. Called in titlePanel
  tags$style(
    HTML(
      ".vertically-centered {
      display: flex;
      align-items: center;
      height: 100%;
    }"
    )
  ),

  theme = bslib::bs_theme(
      preset = 'lumen',
      info = '#2F4F4F',
      primary = '#2F4F4F',
      font_scale = 0.9
  ),

  fluidRow(
    # column(
    #   4,
    #   img(
    #     src = "uvm_logo.png",
    #     height = "40px",
    #     width = "125px"
    #   ),
    #   offset = 0
    # ),
    column(
      4,
      div(class = 'vertically-centered',
          titlePanel("CDAE 1020 Grade Analysis"))
    )
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(


    ## Side Panel ---------------------------------------------------------

    sidebarPanel(

      actionButton(
        'show_help',
        'How to use this app',
        width = '100%',
        style =
          "color: #fff;
           background-color: #2F4F4F;
           border-color: #2F4F4F;
           border-radius: 10px;
           border-width: 2px"
      ),

      # Upload file for analysis
      fileInput(
        'file',
        'Upload Grades (.csv or .xlsx)',
        accept = c('.csv', '.xlsx', '.xls')
      ),

      # Dropdown for column names. Check server for specs
      uiOutput('dropdown_columns'),

      # Clean assignment name
      textInput(
        inputId = 'clean_assignment_name',
        label = 'Clean Assignment Name (Optional)',
        placeholder = '(To display on graphs)'
      ),

      # This one for zeroes
      awesomeCheckbox(
        inputId = "remove_zeroes",
        label = "Remove zeroes from analysis",
        value = FALSE,
        status = 'info'
      ),

      # Remove cohort numbering
      awesomeCheckbox(
        inputId = "split_cohort_names",
        label = "Split TA names and cohort numbers (Optional)",
        value = FALSE,
        status = 'info'
      ),

      # How to split cohorts
      textInput(
        inputId = 'split_on',
        label = 'What character to split on? (Required if splitting)',
        placeholder = 'This is likely a hyphen "-"',
        value = '-'
      ),

      # Name before or after split
      awesomeRadio(
        inputId = "before_or_after",
        label = "Are TA Names before or after the split character? (Required if splitting)",
        choices = c('before', 'after'),
        selected = 'after',
        inline = TRUE,
        status = "info"
      ),

      actionButton(
        'run_analysis',
        'Run Analysis',
        width = '100%',
        style =
          "color: #fff;
           background-color: #243f3f;
           border-color: #243f3f;
           border-radius: 10px;
           border-width: 2px"
      )
    ),


    ## Main Panel ---------------------------------------------------------

    mainPanel(
      tabsetPanel(
        tabPanel('Boxplot', plotlyOutput('boxplot', height = '600px')),
        tabPanel('Scatter', plotlyOutput('scatter_plotly', height = '600px')),
        tabPanel('Means', plotlyOutput('means_graph', height = '600px')),
        tabPanel('Histogram', plotlyOutput('histogram', height = '600px')),
        tabPanel('Summary Table', DT::DTOutput('summary_table')),
        tabPanel('Means Table', DT::DTOutput('means_by_cohort')),
        tabPanel('Tests', fluidRow(
          uiOutput('levene_exp'),
          verbatimTextOutput('levene_test'),
          uiOutput('kruskal_wallis_exp'),
          verbatimTextOutput('kruskal_wallis'),
          uiOutput('dunn_exp'),
          verbatimTextOutput('dunn_test'),
          uiOutput("anderson_paper")
        )),
        tabPanel('Data', DT::DTOutput('df')),
        tabPanel('Read Me', uiOutput('message'))
      )
    )
  )
)



# 2. Server ------------------------------------------------------------------


server <- function(input, output) {

  # From Example
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })


  # Get col names from file -----------------------------------------------

  output$dropdown_columns <- renderUI({
    req(input$file)

    # Load file
    if (tools::file_ext(input$file$name) == "csv") {
      df <- read_csv(input$file$datapath)
    } else if (tools::file_ext(input$file$name) %in% c("xlsx", "xls")) {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Unsupported file format")
    }

    # Get column names
    column_names <- colnames(df) %>%
      str_sub(1, 40)

    # List of UI options to generate
    tagList(
      selectInput('cohorts', 'Select Cohort Column', choices = column_names),
      selectInput('usernames', 'Select Username Column', choices = column_names),
      selectInput('assignment', 'Select Assignment Column', choices = column_names)
    )
  })


  # Get DF --------------------------------------------------------

  rval_df <- eventReactive(input$file, {
    # Make sure file is uploaded
    req(input$file)

    # Read data frame from uploaded file
    if (tools::file_ext(input$file$name) == "csv") {
      df <- read_csv(input$file$datapath)
    } else if (tools::file_ext(input$file$name) %in% c("xlsx", "xls")) {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Unsupported file format")
    }

    # Reduce length of column names so they fit in the dropdown
    colnames(df) <- str_sub(colnames(df), 1, 40)
    df
  })


  # Run analyze_grades ------------------------------------------

  rval_results <- eventReactive(input$run_analysis, {
    result_list <- analyze_grades(
      df = rval_df(),
      cohort_column = as.character(input$cohorts),
      username_column = as.character(input$usernames),
      assignment_column = as.character(input$assignment),
      clean_assignment_name = as.character(input$clean_assignment_name),
      remove_zeroes = input$remove_zeroes,
      print_only = FALSE,
      split_cohort_names = input$split_cohort_names,
      split_character = as.character(input$split_on),
      name_before_or_after = as.character(input$before_or_after)
    )
    result_list
  })


  # Boxplot Potly ------------------------------------------------------------

  output$boxplot <- renderPlotly({
    rval_results()$Boxplot
  })


  # Scatter Plotly ----------------------------------------------------------

  output$scatter_plotly <- renderPlotly({
    rval_results()$scatter_plotly
  })

  # Means --------------------------------------------------------------

  output$means_graph <- renderPlotly({
    rval_results()$Means_Graph
  })


  # Histogram ---------------------------------------------------------------

  output$histogram <- renderPlotly({
    rval_results()$Histogram
  })


  # Summary Table -----------------------------------------------------------

  output$summary_table <- DT::renderDT({
    rval_results()$Summary_Table
  })


  # Means by Cohort ---------------------------------------------------------

  output$means_by_cohort <- DT::renderDT({
    rval_results()$Means_by_cohort
  })


  # Tests -------------------------------------------------------------------

  output$levene_test <- renderPrint({
    rval_results()$Levene_Test
  })

  output$kruskal_wallis <- renderPrint({
    rval_results()$Kruskal_Wallis
  })

  output$dunn_test <- renderPrint({
    rval_results()$Dunn_Test
  })


  # Test Explanations -------------------------------------------------------

  output$levene_exp <- renderText(
    HTML(
      '<br>Keep in mind that if we have cohorts of around 20 students we\'re pushing sample size limits, so interpret with caution.',
      'Also, tests are just a helpful tool so that we can better use our eyeballs and judgment to determine if changes need to be made.',

      "<br><br>First we run the robust Brown-Forsythe Levene-Type Procedure.",
      'The null hypothesis is that there are no differences in variance between',
      'groups. If p < 0.05, we reject the null, conclude that there is unequal',
      'variance between groups and that the Kruskal Wallis test we have lined',
      'up might not be appropriate.<br><br>'
    )
  )

  output$kruskal_wallis_exp <- renderText(
    HTML(
      'Now we run the Kruskal-Wallis test, which is a non-parametric version',
      'of an ANOVA, meaning it can be run on non-normal data. The null hypothesis',
      'is that there are no differences between medians of groups. If p < 0.05,',
      'we reject the null and conclude there are likely differences between',
      'medians.<br><br>'
    )
  )
  output$dunn_exp <- renderText(
    HTML(
      'If the Kruskal-Wallis test comes up significant, it shows that there are',
      'likely differences between medians, but it does not say which groups',
      'are different. The Dunn test is a set of pairwise comparisons between',
      'groups with a p-value adjustment for multiple comparisons. The pairs',
      'shown are those that have significantly different medians.<br><br>'
    )
  )

  url <- a(" Anderson 2020", href="https://uopsych.github.io/psy611/readings/anderson_2019.pdf")

  output$anderson_paper <- renderUI({
    tagList(
      "Psst, read this paper about P values:",
      tags$span(style = "margin-right: 4px;"),
      url
    )
  })

  # output$p_exp <- renderText(
  #   'But also, read this paper about p values: Samantha Anderson '
  # )

  # Motivational Message ----------------------------------------------------

  output$message <- renderUI({
    HTML(
      paste0(
        '<br>',
        rval_results()$message
      )
    )
  })


  # Print --------------------------------------------------------------

  output$df <- DT::renderDT({
    rval_df() %>%
      select(input$usernames, input$cohorts, input$assignment)
  })


  # Help Button -------------------------------------------------------------

  observeEvent(input$show_help, {
    showModal(
      modalDialog(
        HTML(
          'This is a tool to help analyze grades for a single assignment across cohorts.',
          'To begin, Click the <b>Browse</b> button below and navigate to a .csv or .xlsx file.',
          'The file should have at least three columns: one for <b>Cohort</b>, one for <b>Username</b> (can also be first or last name), and one for <b>Assignment</b>.',
          'Once you upload the file, three new fields will appear where you can specify which column is which.',
          'That is all that is required, and you can hit the "Run Analysis" button below!',

          '<br><br>However, there are also some optional settings to make things a little cleaner.',
          'You can provide a <b>Clean Assignment Name</b> to appear on graphs in the event your column name is long and unwieldy.',
          'You can also choose to <b>Remove Zeroes</b>, which makes it easier to see how TAs are actually grading.',

          '<br><br>Finally, you can choose to <b>Split Cohort Names</b> to make them shorter.',
          'This can only work if TA names are separated from numbers with a hyphen, colon, or some other unique character.',
          'For example, something like "Cohort 1 - Some Awesome TA".',
          'If you choose to split names, you must specify which character to split on and whether you want to keep the text before or after that character.',

          '<br><br>Once you run the analysis, you can explore the outputs in the tabs to the right.',
          'Note that all the graphs are interactive, so you can hover over them for more information.',
          'The scatter plot should be particularly helpful for spot-checking grades.',
          'You can also save static images by clicking on the camera symbol at the top of the graph.',
          'And don\'t forget to check the <b>Read Me</b> tab after you run the analysis!',

          '<br><br>Note that this app is only set up to analyze one grade at a time across cohorts.',
          'It can be used to analyze project grades, although the histogram bins get hinky.',
          'Simply choose the project cohorts instead of the regular grading cohorts.',
          'However, it is not set up to analyze an assignment without groups, or multiple assignments at once.',

          '<br><br>Feel free to reach out if there are any bugs or particular features that you would like to be added!',
          '<br><br>christopher.donovan@uvm.edu'
        ),
        title = HTML('<b>Welcome to the CDAE 1020 Grade Analysis Shiny App</b>'),
        easyClose = TRUE,
        size = 'l'
      )
    )
  })
}


# 3. App ---------------------------------------------------------------------


shinyApp(ui = ui, server = server)
