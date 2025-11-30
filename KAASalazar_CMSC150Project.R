library(shiny)
library(shinydashboard)
library(DT)

options(digits = 4)
options(scipen = 999)
options(max.print = 999999)

Simplex = function(tableau,isMax=FALSE){
  
  iteration = 0
  tableauiter = list()
  basicsoliter = list()
  
  n = nrow(tableau)
  n2 = ncol(tableau)
  
  while (any(tableau[n, 1:(n2 - 2)] < 0)) {
    iteration = iteration + 1
    
    if (iteration > 500) {
      stop("Maximum iterations reached.")
    }
    
    minvalue = min(tableau[n, 1:(n2-1)])
    pivotcolumn = which(tableau[n, 1:(n2-1)] == minvalue)[1] #get index of lowest negative no. in the last row
    emptyvector = c()
    
    for(a in 1:(n-1)) {
      if(tableau[a,n2]/tableau[a,pivotcolumn] > 0 && tableau[a,pivotcolumn] != 0 ) { #checks for negative ratios and division by 0
        emptyvector = c(emptyvector,tableau[a,n2]/tableau[a,pivotcolumn]) #gets the test ratios
      }
      else {
        emptyvector = c(emptyvector,100000000000000) #ignore invalid test ratios like negative numbers and dividing with 0 by putting a large number
      }
    }
    minratio = min(emptyvector)
    #infeasibility check
    if (minratio >= 100000000000000) {
      return(list(
        message = "❌ Problem is infeasible",
        finalTableau = round(tableau, 4),
        iteration = iteration,
        tableauiter = tableauiter,
        basicsoliter = basicsoliter
      ))
    }
    pivotrow = which(emptyvector == minratio)[1]  #get the index of pivotrow
    
    tableau[pivotrow,] = tableau[pivotrow,]/tableau[pivotrow,pivotcolumn]
    for(j in 1:n) {
      if (pivotrow == j){
        next
      }
      normalizedrow = tableau[j,pivotcolumn] * tableau[pivotrow,]
      tableau[j,] = tableau[j,] - normalizedrow
    }
    #for basic solution for the iterations
    solnames = colnames(tableau)[1:(n2-1)] #get the column names up to 2nd to the last column
    basicSolution = c()
    for (i in 1:(n2-1)) {
      if (all(tableau[,i] %in% c(0, 1)) && sum(tableau[,i]) == 1) { #Checks if the column is only 0's or 1's and the sum of the column is 1  
        one = which.max(tableau[,i]) #gets the index of the only 1 in that column 
        basicSolution = c(basicSolution,tableau[one,n2])
      }
      else{
        basicSolution = c(basicSolution,0)
      }
    }
    basicSolution = matrix(basicSolution, nrow = 1)
    colnames(basicSolution) = solnames
    
    tableauiter[[iteration]] = round(tableau,4)
    basicsoliter[[iteration]] = round(basicSolution,4)
    
  }
  
  #extracts the last row
  solnames = colnames(tableau)[1:(n2-1)] #get the column names up to 2nd to the last column
  basicSolution = c()
  for (i in 1:(n2-2)) {
    basicSolution = c(basicSolution,tableau[n,i])
  }
  basicSolution = c(basicSolution,tableau[n,n2])
  basicSolution = matrix(basicSolution, nrow = 1)
  colnames(basicSolution) = solnames
  basicsoliter[[iteration]] = round(basicSolution,4)
  
  
  return(list(finalTableau=round(tableau,4),basicSolution = round(basicSolution,4),Z = round(tableau[n,n2],4),
              tableauiter=tableauiter,basicsoliter=basicsoliter))
}



mitigationProjects = data.frame(
  row.names = c("Large Solar Park","Small Solar Installations","Wind Farm","Gas-to-renewables conversion","Boiler Retrofit","Catalytic Converters for Buses","Diesel Bus Replacement","Traffic Signal/Flow Upgrade","Low-Emission Stove Program","Residential Insulation/Efficiency",
                "Industrial Scrubbers","Waste Methane Capture System","Landfill Gas-to-energy","Reforestation (acre-package)","Urban Tree Canopy Program (street trees)","Industrial Energy Efficiency Retrofit","Natural Gas Leak Repair","Agricultural Methane Reduction","Clean Cookstove & Fuel Switching (community scale)","Rail Electrification",
                "EV Charging Infrastructure","Biochar for soils (per project unit)","Industrial VOC","Heavy-Duty Truck Retrofit","Port/Harbor Electrification","Black Carbon reduction","Wetlands restoration","Household LPG conversion program","Industrial process change","Behavioral demand-reduction program"),
  CO2 = c(60,18,55,25,20,30,48,12,2,15,6,28,24,3.5,4.2,22,10,8,3.2,80,20,6,2,36,28,1.8,10,2.5,3,9),
  NO = c(0,0,0,1,0.9,2.8,3.2,0.6,0.02,0.1,0.4,0.2,0.15,0.04,0.06,0.5,0.05,0.02,0.04,2,0.3,0.01,0.01,2.2,1.9,0.02,0.03,0.03,0.02,0.4),
  SO2 = c(0,0,0,0.2,0.4,0.6,0.9,0.1,0.01,0.05,6,0.1,0.05,0.02,0.01,0.3,0.01,0.01,0.02,0.4,0.05,0,0,0.6,0.8,0.01,0.02,0.01,0.01,0.05),
  PM2_5 = c(0,0,0,0.1,0.2,0.8,1,0.4,0.7,0.05,0.4,0.05,0.03,0.01,0.03,0.15,0.01,0.02,0.9,1.2,0.1,0.01,0,0.6,0.7,0.6,0.02,0.4,0,0.05),
  CH4 = c(0,0,0,1.5,0.1,0,0,0.05,0,0.02,0,8,6.5,0.8,0.6,0.2,4,7.2,0.1,0,0,2.5,0,0,0,0.05,3.2,0.05,0,0.01),
  VOC = c(0,0,0,0.5,0.05,0.5,0.7,0.2,0.01,0.02,0.1,0.2,0.1,0.03,0.02,0.1,0.02,0.05,0.02,0.6,0.05,0.01,6.5,0.3,0.2,0.01,0.01,0.02,0,0.3),
  CO = c(0,0,0,2,1.2,5,6,3,1.5,0.5,0.6,0.1,0.05,0.1,0.15,1,0.02,0.02,2,10,0.5,0.01,0.1,4.2,3.6,1,0.05,1.2,0,2.5),
  NH3 = c(0,0,0,0.05,0.02,0.01,0.02,0.02,0.03,0,0.01,0,0,0.01,0.005,0.01,0.02,0.1,0.05,0.02,0.01,0.2,0,0.01,0.01,0.02,0.15,0.03,0,0.01),
  BC = c(0,0,0,0.01,0.01,0.05,0.08,0.02,0.2,0,0.01,0,0,0.005,0.02,0.01,0,0,0.25,0.1,0.01,0,0,0.04,0.03,0.9,0.02,0.1,0,0.01),
  N2O = c(0,0,0,0.3,0.05,0.02,0.03,0.01,0,0.01,0,0.05,0.03,0.005,0.002,0.03,0.01,0.05,0,0.05,0.01,0.02,0,0.02,0.02,0,0.04,0,1.5,0.01),
  Cost = c(4000,1200,3800,3200,1400,2600,5000,1000,180,900,4200,3600,3400,220,300,1600,1800,2800,450,6000,2200,1400,2600,4200,4800,600,1800,700,5000,400)
)

targets = c(CO2 = 1000, NO = 35, SO2 = 25, PM2_5 = 20, CH4 = 60, VOC = 45, CO = 80, NH3 = 12, BC = 6, N2O = 10)


dualtableau = function(mitigationProjects, targets, selected) {
  

  costs = mitigationProjects$Cost[selected]
  reductions = as.matrix(mitigationProjects[selected, 1:10])
  
  nprojects = length(selected)
  npollutants = length(targets)
  
  constraintrows = cbind(
    reductions,                   
    -diag(nprojects),            #slack variables for capacity constraint
    diag(nprojects),             #slack variables
    0,                          
    costs                        
  )
  
  objectiverow = c(  #last row must have -targets
    -targets,                     
    rep(20, nprojects),          
    rep(0, nprojects),           
    1,                            
    0                             
  )
  
  tableau = rbind(constraintrows, objectiverow)
  
  colnames(tableau) = c(
    paste0("s", 1:(npollutants + nprojects)),
    paste0("x", 1:nprojects),
    "Z", "RHS"
  )
  rownames(tableau) = NULL
  return(tableau)
}

#Get the projects that are not zero from the solution
getResultsTable = function(simplexResult, selectedProjects, mitigationProjects) {
  basicSol = simplexResult$basicSolution
  x_columns = grep("^x", colnames(basicSol)) #Find column names that start with x(the projects)
  projectUnits = as.numeric(basicSol[1, x_columns]) #get the amount of times each project is implemented
  projectNames = rownames(mitigationProjects)[selectedProjects]
  projectCosts = mitigationProjects$Cost[selectedProjects]
  nonZero = which(projectUnits > 0)  #get the projects that are non zero
  
  if (length(nonZero) == 0) { #just a safety check
    return(data.frame(Message = "No projects"))
  }
  #build the table
  resultsTable = data.frame(
    "Mitigation Project" = projectNames[nonZero],
    "Number of Project Units" = round(projectUnits[nonZero], 4),
    "Cost($)" = round(projectUnits[nonZero] * projectCosts[nonZero], 2),
    check.names = FALSE
  )
  
  return(resultsTable)
}



#GREENVALE CITY POLLUTION REDUCTION PLAN
ui <- dashboardPage(
  dashboardHeader(title = "POLLUTION PLAN"),
  
  dashboardSidebar(
    width = 400,
    h4("Choose Projects"),
    
    selectInput(
      inputId = "proj",
      label = NULL,
      choices = rownames(mitigationProjects),
      multiple = TRUE
    ),
    
    actionButton("select_all", "Select All"),
    actionButton("reset", "Reset"),
    br(), br(),
    actionButton("solve", "Solve")
  ),
  
  dashboardBody(
    tags$head(    #styled with CSS
      tags$style(HTML("
    body, .content-wrapper, .main-sidebar {
      background-color: #000013;
      color: #FFFFFF;
    }
    
    .content-wrapper {
      background-color: #5C5C68;
    }
    
    .main-sidebar,
    .main-header .navbar,
    .main-header .logo {
      background-color: #19719C;
      color: #FFFFFF;
    }
    
    h3, h4 {
      color: #FFFFFF;
      font-weight: 500;
    }
    
    .btn {
      background-color: #19719C;
      color: #FFFFFF;
      border: none;
      border-radius: 4px;
      padding: 8px 16px;
      font-weight: 500;
    }
    
    .btn:hover {
      background-color: #83B3CA;
      color: #000013;
    }


    button.btn.btn-active {
      box-shadow: 0 4px 12px rgba(0,0,0,0.6) !important;
      transform: translateY(-2px) !important;
    }
    
    .dataTables_wrapper {
      background-color: #5C5C68;
      color: #FFFFFF;
      padding: 10px;
      border-radius: 6px;
    }
    
    table.dataTable {
      background-color: #5C5C68;
      color: #FFFFFF;
    }
    
    table.dataTable thead th {
      background-color: #19719C;
      color: #FFFFFF;
      border-bottom: 2px solid #83B3CA;
    }
    
    table.dataTable tbody tr:hover {
      background-color: #83B3CA;
      color: #000013;
    }
    .dataTables_length select {
      background-color: #19719C !important;
      color: #FFFFFF !important;
      border: 1px solid #83B3CA !important;
      padding: 4px 8px !important;
    }
    
    .dataTables_length select option {
      background-color: #19719C !important;
      color: #FFFFFF !important;
    }
    
    /* Also style the label text */
    .dataTables_length label {
      color: #FFFFFF !important;
    }
        
  ")),
      #just a subtle shadow to let you know you are in that section
      tags$script(HTML("
    $(document).on('click', 'button[id^=\"btn\"]', function() {
      $('button[id^=\"btn\"]').removeClass('btn-active');
      $(this).addClass('btn-active');
    });
  "))
    ),
    
    h3("Selected Projects"),
    DT::DTOutput("selectedProjectsTable"),
    
    fluidRow(  #buttons
      actionButton("btnresults", "Results"),
      actionButton("btniterations", "Tableau Iterations"),
      actionButton("btnsolution", "Final Solution & Z"),
      actionButton("btninitial", "Initial Tableau"),
      actionButton("btnfinal", "Final Output Table")
    ),
    
    br(), #adds some space between elements
    
    #Output as Datatables 
    DT::DTOutput("display"),
    uiOutput("iterationselector"),
    DT::DTOutput("itertableau"),
    DT::DTOutput("iterbasic")
  )
)
server <- function(input, output, session) {
  
  #Select all
  observeEvent(input$select_all, {
    updateSelectInput(session, "proj", selected = rownames(mitigationProjects))
  })
  
  solved = reactiveVal(NULL)
  activeView = reactiveVal("results") #default activeView
  
  #Reset
  observeEvent(input$reset, {
    updateSelectInput(session, "proj", selected = character(0))
    solved(NULL)
    activeView("results")
  })
  
  #output the selectedprojects as DT
  output$selectedProjectsTable = DT::renderDT({
    if (length(input$proj) == 0) {
      return(data.frame(Message = "No projects selected"))
    }
    
    selected = match(input$proj, rownames(mitigationProjects))
    
    projectData = mitigationProjects[selected, ]
    projectData = cbind(Project = rownames(projectData), projectData)
    rownames(projectData) = NULL
    
    datatable(
      projectData,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color: white; font-weight: bold;",
        "Selected Projects and Their Pollutant Reductions (per unit)"
      )
    )
  })
  
  #Solve
  observeEvent(input$solve, {
    if (length(input$proj) == 0) {
      solved(list(message = "No projects selected."))
      activeView("results")
      return()
    }
    
    #Get the selected projects from the dataframe
    selectedprojects = match(input$proj, rownames(mitigationProjects)) 
    
    #build the tableau and feed into our simplex
    tableau = dualtableau(mitigationProjects, targets, selectedprojects)
    simplexresult = Simplex(tableau)
    
    #Check infeasibility 
    if (!is.null(simplexresult$message)) {
      solved(list(
        message = simplexresult$message,
        simplex = simplexresult,
        initial = tableau  
      ))
      activeView("results")
      return()
    }
    
    #store the data on solved
    solved(list(
      initial = tableau, 
      simplex = simplexresult,
      selectedProjects = selectedprojects
    ))
    activeView("results") #default activeView
  })
  
  #If the problem was feasible activeView will change depending on the button pressed
  observeEvent(input$btninitial,   { req(solved()); activeView("initial") })
  observeEvent(input$btnresults,   { req(solved()); activeView("results") })
  observeEvent(input$btnsolution,  { req(solved()); activeView("solution") })
  observeEvent(input$btniterations,{ req(solved()); activeView("iterations") })
  observeEvent(input$btnfinal,     { req(solved()); activeView("final") })
  
  currentdisplay = reactive({
    req(solved(), activeView())
    resultingdata = solved()
    view = activeView()
    
    #Handle infeasible cases (can view initial tableau and iterations)
    #results and other views would have the same message
    if (!is.null(resultingdata$message)) {
      if (view == "results") {
        return(data.frame(Message = resultingdata$message))
      }
      if (view == "iterations") {
        return(NULL)  
      }
      if (view == "initial") {
        return(resultingdata$initial)
      }
      return(data.frame(Message = "❌ Problem infeasible. Check the Results tab for details."))
    }
    
    switch(
      view,
      initial = resultingdata$initial,
      solution = cbind(Solution = resultingdata$simplex$basicSolution, Z = resultingdata$simplex$Z),
      results = getResultsTable(resultingdata$simplex, resultingdata$selectedProjects, mitigationProjects),
      final = resultingdata$simplex$finalTableau,
      iterations = NULL
    )
  })
  
  output$display = DT::renderDT({
    validate(need(solved(), "Click Solve to generate a tableau."))
    data = currentdisplay()
    
    #if it's the results view show a customized table
    if (activeView() == "results" && !is.null(solved()$simplex) && is.null(solved()$message)) {
      totalCost = solved()$simplex$Z
      
      datatable(
        data,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color: white;',
          htmltools::tags$div(
            style = 'margin-bottom: 20px;',
            htmltools::tags$h4('The Optimized Cost', style = 'margin-bottom: 5px; font-weight: bold;'),
            htmltools::tags$p(
              style = 'margin-bottom: 15px; font-size: 16px;',
              'The cost of this optimal mitigation project is ',
              htmltools::tags$strong(
                sprintf('$%.2f', totalCost),
              )
            ),
            htmltools::tags$h4('The Solution and Cost Breakdown by Mitigation Project', 
                               style = 'margin-top: 15px; font-weight: bold;')
          )
        ),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 't',
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-right', targets = 1:2)
          )
        )
      ) 
    } else { #if any other view just the default 
      datatable(data,options = list(pageLength = 15, scrollX = TRUE),
      )
    }
  })
  
  #custom dropdown for the iterations
  output$iterationselector = renderUI({
    req(solved(), activeView() == "iterations")
    
    if (is.null(solved()$simplex)) {
      return(div("No iterations available."))
    }
    
    simplexdata = solved()$simplex
    niter = length(simplexdata$tableauiter)
    
    if (niter == 0) {
      return(div("No iterations recorded."))
    }
    
    selectInput("selectediter", "Choose Iteration", choices = seq_len(niter), selected = niter)
  })
  
  output$itertableau = DT::renderDT({
    req(solved(), activeView() == "iterations", input$selectediter)
    req(solved()$simplex) 
    
    solved()$simplex$tableauiter[[as.numeric(input$selectediter)]]
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$iterbasic = DT::renderDT({
    req(solved(), activeView() == "iterations", input$selectediter)
    req(solved()$simplex)  
    
    solved()$simplex$basicsoliter[[as.numeric(input$selectediter)]]
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}

shinyApp(ui, server)