# Load packages ----------------------------------------------------------------

library(shiny)
library(DT)
library(sortable)

# Load sources -----------------------------------------------------------------

source("functions.R")

# Define tissue list -----------------------------------------------------------



# Define UI --------------------------------------------------------------------

ui <- fluidPage(  
  tags$head(
    tags$style(HTML("
      body {
        transform: scale(1.0); /* Adjust the scale factor */
        transform-origin: top left; /* Keep scaling from the top-left corner */
      }
      .bucket-list-container {min-height: 350px;}
    "))
  ),
  ## *style Tags-----------------------------------------------------------------
  tags$head(tags$style(".modal-dialog{ width:1000px}")),
  tags$head(tags$style(".modal-body{ min-height:800px}")),
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"),
    tags$style(HTML("
      .irs-grid {
        display: none; /* Hide the grid lines */
      }
      .irs-slider {
        width: 20px; /* Customize slider handle size */
        height: 20px;
      }
      .irs-bar {
        background-color: #007bff; /* Customize the bar color */
        border: none;
      }
      .irs-bar-edge {
        background-color: #007bff; /* Match bar edge color to bar */
        border: none;
      }
      .irs-single {
        background: #007bff; /* Customize the tooltip color */
        color: white;
      }
      .irs-dots {
        display: none; /* Hide extra tick marks */
      }
      .irs-line {
        height: 6px; /* Customize the line thickness */
        background: #ddd; /* Line background color */
        border: none;
      }
      .irs-min, .irs-max {
        display: none; /* Hide min and max labels if not needed */
      }
    ")))
  ),
  
  
  titlePanel("MT Maker"),
  tabsetPanel( #Could move this so that the tab is within the side panel, instead of a layer above
    tabPanel("Study Editor",
             ## *study editor panel----------------------------------------------
               mainPanel(
                 
                 textInput("study_name", "Enter Study name:", ""),
                 
#!_________________Just override all conditions and display all panels, lazy fix! 
                 # Enter Study GID
                 conditionalPanel(
                  condition = "input.study_name !== null && input.study_name !== ''",
                  textInput("study_gid", "Enter Study GID:", "")
                 ),
                 
                 # Select GC Model
                 conditionalPanel(
                   condition = "input.study_gid !== null && input.study_gid !== ''",
                   selectInput("gc_model", "Select GC Model:",
                               choices = c("", "1480", "2470", "2480"))
                 ),

                # Group name
                conditionalPanel(
                  condition = "input.gc_model !== null && input.gc_model !== ''",
                  textInput("group_name", "Enter Group Name (e.g., G4):", "")
                ),
                
                # Timepoint
                conditionalPanel(
                  condition = "input.group_name !== null && input.group_name !== ''",
                  textInput("timepoint", "Enter Timepoint (e.g., 168H):", "")
                ),
                 
                 # Number of animals
                 conditionalPanel(
                   condition = "input.timepoint !== null && input.timepoint !== ''",
                   selectInput("num_animals", "How many animals?",
                               choices = c("", 1, 2, 3, 4, 5))
                 ),
                 
                 # Ask if user wants to assign animal IDs or labels
                 conditionalPanel(
                   condition = "input.timepoint !== null && input.timepoint !== ''", 
                   selectInput("assign_ids", "Would you like to assign animal ID's or label animals 'A, B, C, D, E'?",
                               choices = c("", "Use Labels", "Assign IDs"))
                 ),
                 
                 # If assigning IDs, ask for IDs for each animal
                 conditionalPanel(
                   condition = "input.assign_ids === 'Assign IDs' && input.num_animals !== null && input.num_animals !== ''", 
                   uiOutput("animal_ids_input")
                 ),
                 
                 
                 # New Question: Add rack backgrounds and skip racks
                 conditionalPanel(
                   condition = "input.assign_ids !== null && input.assign_ids !== ''",
                   selectInput("rack_option", "Add rack backgrounds and skip racks?",
                               choices = c("", "No", "Yes"))
                 ),
#_______________REMOVE BUTTON?
                 # Add the button that triggers table generation, conditionally displayed
                 conditionalPanel(
                   condition = "(input.rack_option !== null && input.rack_option !== '' ) || (input.gc_model != 2470 && ((input.new_rack_per_animal !== null
                            && input.new_rack_per_animal !== '') || (input.new_rack_per_tissue !== null && input.new_rack_per_tissue !== ''))",
                   actionButton("generate_mt", "Generate MT")
                 ),
#_______________ADD MAIN PANEL VERBOSE TEXT OUTPUT OF STUDY DETAILS
               )
               
               
             
    )## *sample editor panel----------------------------------------------
    ,tabPanel("Sample Editor",
            
              
              sidebarLayout(
                
                sidebarPanel(
                  sliderInput("numLists", "Enter number of tissue lists to be combined", 
                                min = 1, max = 4, value = 1, step = 1, ticks = FALSE), 
                  
                  tabsetPanel(
                    id = "sidebar_tabs",
                    
                    tabPanel("Tab_1",
                      # Tissue input using tokenized UI
                      
                      selectizeInput(
                        "tissues_1",
                        "Enter List of Tissues (select or add new):",
                        choices = tissue_options,
                        multiple = TRUE,
                        options = list(
                          create = TRUE,  # Allow user to add custom entries
                          placeholder = "Type or select tissues...",
                          maxItems = NULL  # Allow unlimited selections
                        )
                       ),
                      
                      # Group by subject or tissues
                      conditionalPanel(
                        condition = "1 === 2",
                        selectInput("group_by_1", "Group by subject or tissues?",
                                    choices = c("", "Subject", "Tissues"))
                      ),
                      
                      #New rack IF SUBJECT
                      conditionalPanel(
                        condition = "input.group_by_1 == 'Subject'",
                        selectInput("new_rack_per_animal_1", "Make a new rack when the subject changes?",
                                    choices = c("", "No", "Yes"))
                      ),
                      
                      #New rack IF TISSUE
                      conditionalPanel(
                        condition = "input.group_by_1 == 'Tissues'",
                        selectInput("new_rack_per_tissue_1", "Make a new rack when the tissue changes?",
                                    choices = c("", "No", "Yes, 1 tissue per rack", "Yes, 2 tissues per rack"))
                      )       
                    ),
                    
                    tabPanel("Tab_2",
                      # Tissue input using tokenized UI
                      selectizeInput(
                        "tissues_2",
                        "Enter List of Tissues (select or add new):",
                        choices = tissue_options,
                        multiple = TRUE,
                        options = list(
                          create = TRUE,  # Allow user to add custom entries
                          placeholder = "Type or select tissues...",
                          maxItems = NULL  # Allow unlimited selections
                        )
                       ), 
                      
                      # Group by subject or tissues
                      conditionalPanel(
                        condition = "input.tissues_2 !== null && input.tissues_2.length > 0",
                        selectInput("group_by_2", "Group by subject or tissues?",
                                    choices = c("", "Subject", "Tissues"))
                      ),
                      
                      #New rack IF SUBJECT
                      conditionalPanel(
                        condition = "input.group_by_2 !== null && input.group_by_2 !== '' &&  input.group_by_2 == 'Subject'",
                        selectInput("new_rack_per_animal_2", "Make a new rack when the subject changes?",
                                    choices = c("", "No", "Yes"))
                      ),
                      
                      #New rack IF TISSUE
                      conditionalPanel(
                        condition = "input.group_by_2 !== null && input.group_by_2 !== '' &&  input.group_by_2 == 'Tissues'",
                        selectInput("new_rack_per_tissue_2", "Make a new rack when the tissue changes?",
                                    choices = c("", "No", "Yes, 1 tissue per rack", "Yes, 2 tissues per rack"))
                      )       
                    ),
                  
                    tabPanel("Tab_3",
                      # Tissue input using tokenized UI
                      selectizeInput(
                        "tissues_3",
                        "Enter List of Tissues (select or add new):",
                        choices = tissue_options,
                        multiple = TRUE,
                        options = list(
                          create = TRUE,  # Allow user to add custom entries
                          placeholder = "Type or select tissues...",
                          maxItems = NULL  # Allow unlimited selections
                        )
                       ), 
                      
                      # Group by subject or tissues
                      conditionalPanel(
                        condition = "input.tissues_3 !== null && input.tissues_3.length > 0",
                        selectInput("group_by_3", "Group by subject or tissues?",
                                    choices = c("", "Subject", "Tissues"))
                      ),
                      
                      #New rack IF SUBJECT
                      conditionalPanel(
                        condition = "input.group_by_3 !== null && input.group_by_3 !== '' &&  input.group_by_3 == 'Subject'",
                        selectInput("new_rack_per_animal_3", "Make a new rack when the subject changes?",
                                    choices = c("", "No", "Yes"))
                      ),
                      
                      #New rack IF TISSUE
                      conditionalPanel(
                        condition = "input.group_by_3 !== null && input.group_by_3 !== '' &&  input.group_by_3 == 'Tissues'",
                        selectInput("new_rack_per_tissue_3", "Make a new rack when the tissue changes?",
                                    choices = c("", "No", "Yes, 1 tissue per rack", "Yes, 2 tissues per rack"))
                      )       
                    ),
                  
                    tabPanel("Tab_4",
                      # Tissue input using tokenized UI
                      selectizeInput(
                        "tissues_4",
                        "Enter List of Tissues (select or add new):",
                        choices = tissue_options,
                        multiple = TRUE,
                        options = list(
                          create = TRUE,  # Allow user to add custom entries
                          placeholder = "Type or select tissues...",
                          maxItems = NULL  # Allow unlimited selections
                        )
                       ), 
                      
                      # Group by subject or tissues
                      conditionalPanel(
                        condition = "input.tissues_4 !== null && input.tissues_4.length > 0",
                        selectInput("group_by_4", "Group by subject or tissues?",
                                    choices = c("", "Subject", "Tissues"))
                      ),
                      
                      #New rack IF SUBJECT
                      conditionalPanel(
                        condition = "input.group_by_4 !== null && input.group_by_4 !== '' &&  input.group_by_4 == 'Subject'",
                        selectInput("new_rack_per_animal_4", "Make a new rack when the subject changes?",
                                    choices = c("", "No", "Yes"))
                      ),
                      
                      #New rack IF TISSUE
                      conditionalPanel(
                        condition = "input.group_by_4 !== null && input.group_by_4 !== '' &&  input.group_by_4- == 'Tissues'",
                        selectInput("new_rack_per_tissue_4", "Make a new rack when the tissue changes?",
                                    choices = c("", "No", "Yes, 1 tissue per rack", "Yes, 2 tissues per rack"))
                      )      
                    ),
                  )
                ),
                
                mainPanel(
                  h4("Sample Sorter"),
                  uiOutput("tissueSorter")
                )
              )
    ## *Preview panel----------------------------------------------         
    ),tabPanel("Preview and Export",
          mainPanel(
            h4("Preview of MT:"),
                 
            # Show the preview table only after the button is clicked
            conditionalPanel(
              condition = "output.preview_ready",
              DTOutput("preview_table")
            )
          )       
     )
  )
)

# Define the server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # tab panel hider for dynamic tissue list 
  observeEvent(input$numLists, {
    num_tabs <- input$numLists
    
    # Generate the list of tab names to show
    tabs_to_show <- paste0("Tab_", seq_len(num_tabs))
    
    # Update the tabsetPanel to only show the specified tabs
    for (i in 1:4) {
      tab_id <- paste0("Tab_", i)
      
      if (tab_id %in% tabs_to_show) {
        showTab(inputId = "sidebar_tabs", target = tab_id)
      } else {
        hideTab(inputId = "sidebar_tabs", target = tab_id)
      }
    }
  })  
  
  #*Bucket List (rendered in sample editor main panel) ------------------------------------------------------
  
  #reactive logic to create a tissueList variable that contains ordered
  # small and large tissue lists, that can be accessed throughout server()
  tissueLists <- reactiveValues(
    Tab_1 = list(smallTissues = character(), largeTissues = character()),
    Tab_2 = list(smallTissues = character(), largeTissues = character()),
    Tab_3 = list(smallTissues = character(), largeTissues = character()),
    Tab_4 = list(smallTissues = character(), largeTissues = character())
  )
  
  # Observer to sync tissues_X with smallTissues and handle new entries
  observe({
    
    current_tab <- input$sidebar_tabs
    if (!is.null(current_tab)) {
      tissue_input <- paste0("tissues_", gsub("Tab_", "", current_tab))
      
      # Update smallTissues to include new tissues while preserving existing ones
      if (!is.null(input[[tissue_input]])) {
        new_tissues <- input[[tissue_input]]
        current_small <- tissueLists[[current_tab]]$smallTissues
        current_large <- tissueLists[[current_tab]]$largeTissues
        
        # Add new tissues that are not already present in smallTissues or largeTissues
        remaining_tissues <- setdiff(new_tissues, c(current_small, current_large))
        tissueLists[[current_tab]]$smallTissues <- c(current_small, remaining_tissues)
        
        # Remove any tissues that are no longer in tissues_X
        tissueLists[[current_tab]]$smallTissues <- intersect(tissueLists[[current_tab]]$smallTissues, new_tissues)
        tissueLists[[current_tab]]$largeTissues <- intersect(tissueLists[[current_tab]]$largeTissues, new_tissues)
      }
    }
  })

  # Observer to update smallTissues and largeTissues when items are dragged
  observeEvent(input[[paste0("smallTissuesTemp_", gsub("Tab_", "", input$sidebar_tabs))]], {
    current_tab <- input$sidebar_tabs
    if (!is.null(current_tab)) {
      temp_small <- input[[paste0("smallTissuesTemp_", gsub("Tab_", "", current_tab))]]
      tissueLists[[current_tab]]$smallTissues <- temp_small
    }
  })
  
  observeEvent(input[[paste0("largeTissuesTemp_", gsub("Tab_", "", input$sidebar_tabs))]], {
    current_tab <- input$sidebar_tabs
    if (!is.null(current_tab)) {
      temp_large <- input[[paste0("largeTissuesTemp_", gsub("Tab_", "", current_tab))]]
      tissueLists[[current_tab]]$largeTissues <- temp_large
    }
  })
  
  
  #sample size / reorder popup 
  output$tissueSorter <- renderUI({
    
    current_tab <- input$sidebar_tabs
      
    if (!is.null(current_tab)){
      
      title = "Edit Sample Size and order"
      tab <- paste0(gsub("Tab_", "", current_tab))
      
      fluidPage(
          tags$b("Reorder and assign sample size by dragging elements below"),
          bucket_list(
            header = NULL,
            group_name = "bucket_list_group",
            orientation = "horizontal",
            add_rank_list(
              text = "Small tube",
              labels = tissueLists[[current_tab]]$smallTissues,
              input_id = paste0("smallTissuesTemp_", tab),
              options = sortable_options(multiDrag = TRUE)
            ),
            add_rank_list(
               text = "Large tube",
               labels = tissueLists[[current_tab]]$largeTissues,
               input_id = paste0("largeTissuesTemp_", tab),
               options = sortable_options(multiDrag = TRUE)
            )
          ),
          tags$p("smallTissues"),
          verbatimTextOutput("smallTissuesList"),
          
          tags$p("largeTissues"),
          verbatimTextOutput("largeTissuesList")
      )
    }
  })
  
  output$smallTissuesList <- renderPrint({
    current_tab <- input$sidebar_tabs
    if (!is.null(current_tab)) {
      tissueLists[[current_tab]]$smallTissues
    }
  })

  output$largeTissuesList <- renderPrint({
    current_tab <- input$sidebar_tabs
    if (!is.null(current_tab)) {
      tissueLists[[current_tab]]$largeTissues
    }
  })
  
  # Reactive value to track if preview is ready
  preview_ready <- reactiveVal(FALSE)
  
  # Observe the button click and generate the table
  observeEvent(input$generate_mt, {
    preview_ready(TRUE)
  })
  
  # Make the reactive value accessible to the UI
  output$preview_ready <- reactive({
    preview_ready()
  })
  outputOptions(output, "preview_ready", suspendWhenHidden = FALSE)
  
  # Generate animal ID inputs dynamically based on the number of animals
  output$animal_ids_input <- renderUI({
    req(input$num_animals)
    num_animals <- as.numeric(input$num_animals)
    animal_id_inputs <- lapply(1:num_animals, function(i) {
      textInput(paste0("animal_id_", i), paste("Enter ID for Animal", i), "")
    })
    do.call(tagList, animal_id_inputs)
  })
  
  # Generate preview of tissues in the textarea
  observe({
    updateTextInput(session, "tissue_preview",
                    value = paste(input$tissues, collapse = ", "))
  })
  
  # Generate data based on user input
  generate_data <- reactive({
    
    print(input$smallTissues)
    print(input$largeTissues)
    
    #If 2470, then rack_option is not required (as it is conditional on the model)
    if(input$gc_model == "2470"){
      req(input$num_animals, input$group_name, input$timepoint, input$study_gid)
    }
    else{
      req(input$num_animals, input$group_name, input$timepoint, input$study_gid, input$rack_option)
    }
    
    
    num_animals <- as.numeric(input$num_animals)
    tissues <- input$tissues
    group_name <- input$group_name
    timepoint <- input$timepoint
    assign_ids <- input$assign_ids
    study_gid <- input$study_gid
    
    # Assign animal IDs
    animal_ids <- if (assign_ids == "Use Labels") {
      paste0(study_gid, "-", LETTERS[1:num_animals])  # Prepend study_gid to each animal label
    } else {
      sapply(1:num_animals, function(i) paste0(study_gid, input[[paste0("animal_id_", "-", i)]]))  # Prepend study_gid to custom IDs
    }
    
    # Create subject data
    subject_data <- do.call(rbind, lapply(animal_ids, function(animal_id) {
      do.call(rbind, lapply(tissues, function(tissue) {
        data.frame(
          animal_id = animal_id,
          group = group_name,
          Timepoint = timepoint,
          Tissue = tissue,
          stringsAsFactors = FALSE
        )
      }))
    }))
    
    
    # Sort the subject data rows based on user selection
    #if (input$group_by == "Tissues") {
    # subject_data <- subject_data[order(match(subject_data$Tissue, tissues)), ]
    #} else {
    #  subject_data <- subject_data[order(subject_data$animal_id, match(subject_data$Tissue, tissues)), ]
    #}
    
    # *Assign rack and position -----------------------------------------------------------------------------
    if (nrow(subject_data) > 0) {
      
      total_tissues <- nrow(subject_data)
      
      #2480
      if (input$gc_model == "2480") {
        
        if (input$group_by == "Tissues"){
          
          #sort by tissue
          subject_data <- subject_data[order(match(subject_data$Tissue, tissues)), ]
          
          #by Tissue, yes new rack for 1 tissue
          if(input$new_rack_per_tissue == "Yes, 1 tissue per rack"){
            
            # Initialize rack and position
            rack <- 3
            position <- 1
            tissueCounter <- 0
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              
              
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                tissueCounter <- tissueCounter + 1
              }
              
              # Check if the tissue changes
              if (i < total_tissues && tissueCounter > 0) {
                tissueCounter <- 0
                rack <- rack + 1  # Start a new rack when 2 tissues change
                position <- 1  # Reset position for new rack
              } 
              else {
                # Increment position and wrap to a new rack if needed
                position <- position + 1
                if (position > 10) {
                  rack <- rack + 1
                  position <- 1
                }
              }
            }
          }
          #by Tissue, yes new rack every 2 tissues
          else if(input$new_rack_per_tissue == "Yes, 2 tissues per rack"){
            
            #Sort whole data by tissue
            #subject_data <- subject_data[order(match(subject_data$Tissue, tissues)), ]
            
            # Initialize rack and position
            rack <- 3
            position <- 1
            tissueCounter <- 0
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              
              
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                tissueCounter <- tissueCounter + 1
              }
              
              # Check if the tissue changes
              if (i < total_tissues && tissueCounter > 1) {
                tissueCounter <- 0
                rack <- rack + 1  # Start a new rack when 2 tissues change
                position <- 1  # Reset position for new rack
              } 
              else {
                # Increment position and wrap to a new rack if needed
                position <- position + 1
                if (position > 10) {
                  rack <- rack + 1
                  position <- 1
                }
              }
            }
          }
          #by Tissue, no new rack
          else if (input$new_rack_per_tissue == "No"){
            
            # Initialize rack and position
            rack <- 3
            position <- 1
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              
              # Increment position and wrap to a new rack if needed
              position <- position + 1
              if (position > 10) {
                rack <- rack + 1
                position <- 1
              }
              
            }
          }
        }
        
        else if (input$group_by == "Subject"){ 
          
          #sort by subject
          subject_data <- subject_data[order(subject_data$animal_id, match(subject_data$Tissue, tissues)), ]
          
          #by Subject, yes new rack
          if(input$new_rack_per_animal == "Yes"){
            
            # Initialize rack and position
            rack <- 3
            position <- 1
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              
              # Check if the tissue changes
              if (i < total_tissues && subject_data$animal_id[i] != subject_data$animal_id[i + 1]) {
                rack <- rack + 1  # Start a new rack when tissue changes
                position <- 1  # Reset position for new rack
              } else {
                # Increment position and wrap to a new rack if needed
                position <- position + 1
                if (position > 10) {
                  rack <- rack + 1
                  position <- 1
                }
              }
            }
          }
          
          #by subject, no new rack
          else if (input$new_rack_per_animal == "No"){
            
            # Initialize rack and position
            rack <- 3
            position <- 1
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              
              # Increment position and wrap to a new rack if needed
              position <- position + 1
              
              if (position > 10) {
                rack <- rack + 1
                position <- 1
              }
              
            }
          }
        }
      } 
      
      #2470
      else if (input$gc_model == "2470") {
        
        if (input$group_by == "Tissues"){
          
          #sort by tissue
          subject_data <- subject_data[order(match(subject_data$Tissue, tissues)), ]
          
          if(input$new_rack_per_tissue == "Yes, 1 tissue per rack"){
            
            # Initialize rack and position
            rack <- 3
            position_index <- 1
            tissueCounter <- 0
            
            # Define the allowed positions in each rack
            allowed_positions <- c(1, 3, 5, 7, 9)
            max_positions <- length(allowed_positions)
            
            for (i in seq_len(total_tissues)) {
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- allowed_positions[position_index]
              
              # Check if the tissue changes
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                tissueCounter <- tissueCounter + 1
              }
              
              if (i < total_tissues && tissueCounter > 0) {
                tissueCounter <- 0
                rack <- rack + 1  # Start a new rack when 2 tissues change
                position_index <- 1  # Reset position index for new rack
              } else {
                # Increment position index and wrap to a new rack if needed
                position_index <- position_index + 1
                if (position_index > max_positions) {
                  rack <- rack + 1
                  position_index <- 1
                }
              }
            }
          }
          else if(input$new_rack_per_tissue == "Yes, 2 tissues per rack"){           
            #THIS DOES NOT WORK AND I GIVE UP IT DOESNT REALLY MAKE SENSE FOR 2470 ANYWAYS
            #AND MAYBE SHOULDNT BE AN OFFERED OPTION BUT IM TOO LAZY TO MAKE ANOTHER
            #CONDITIONAL PANEL FOR THE 2470 OOPSS!
            
            # Initialize rack and position
            rack <- 3
            position_index <- 1
            
            
            # Define the allowed positions in each rack
            allowed_positions <- c(1, 3, 5, 7, 9)
            max_positions <- length(allowed_positions)
            
            for (i in seq_len(total_tissues)) {
              
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- allowed_positions[position_index]
              
              # Check if the tissue changes
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1] && input$num_animals < 3) {
                tissueCounter <- tissueCounter + 1
              }
              
              if (i < total_tissues && tissueCounter > 1) {
                tissueCounter <- 0
                rack <- rack + 1  # Start a new rack when 2 tissues change
                position_index <- 1  # Reset position index for new rack
              } 
              else {
                
                tissueCounter1 <- 0
                if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                  tissueCounter1 <- tissueCounter1 + 1
                }
                #basically if you cant fit 2 sets of 2 tissues in the rack nicely, just do it one tissue per rack
                if (i < total_tissues && tissueCounter1 > 0) {
                  tissueCounter1 <- 0
                  rack <- rack + 1  # Start a new rack when 2 tissues change
                  position_index <- 1  # Reset position index for new rack
                } 
                else {
                  
                  # Increment position index and wrap to a new rack if needed
                  position_index <- position_index + 1
                  
                  if (position_index > max_positions) {
                    rack <- rack + 1
                    position_index <- 1
                  }
                }
              }
              
            }
          }
          else if (input$new_rack_per_tissue == "No"){
            total_tissues <- nrow(subject_data)
            subject_data$Rack <- as.integer(3 + (seq_len(total_tissues) - 1) %/% 5)
            subject_data$Pos <- as.integer(c(1, 3, 5, 7, 9)[(seq_len(total_tissues) - 1) %% 5 + 1])
          }
          
        }
        else if (input$group_by == "Subject"){
          
          #sort by subject
          subject_data <- subject_data[order(subject_data$animal_id, match(subject_data$Tissue, tissues)), ]
          
          if(input$new_rack_per_animal == "Yes"){
            
            rack <- 3  # Initial rack number
            pos_index <- 1  # Index to track position in c(1, 3, 5, 7, 9)
            positions <- c(1, 3, 5, 7, 9)  # Allowed positions
            
            for (i in seq_len(total_tissues)) {
              # Assign the current rack and position
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- positions[pos_index]
              
              # Check if the subject changes or if we've filled the current rack
              if (i < total_tissues && subject_data$animal_id[i] != subject_data$animal_id[i + 1]) {
                rack <- rack + 1  # Move to the next rack
                pos_index <- 1  # Reset position index
              } else {
                pos_index <- pos_index + 1  # Move to the next position in the current rack
                
                # If all positions in the rack are filled, move to the next rack
                if (pos_index > length(positions)) {
                  rack <- rack + 1
                  pos_index <- 1  # Reset position index
                }
                
              }
              
            }
            
          }
          else if (input$new_rack_per_animal == "No"){
            total_tissues <- nrow(subject_data)
            subject_data$Rack <- as.integer(3 + (seq_len(total_tissues) - 1) %/% 5)
            subject_data$Pos <- as.integer(c(1, 3, 5, 7, 9)[(seq_len(total_tissues) - 1) %% 5 + 1])
          }
        }
      }
      
      #1480
      else if (input$gc_model == "1480"){
        
        if (input$group_by == "Tissues"){
          
          #sort by tissue
          subject_data <- subject_data[order(match(subject_data$Tissue, tissues)), ]
          
          if(input$new_rack_per_tissue == "Yes, 1 tissue per rack"){
            
            rack <- 3  # Initial rack number
            pos_index <- 1  # Index to track position in c(1, 3, 5, 7, 9)
            positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  # Allowed positions
            
            for (i in seq_len(total_tissues)) {
              # Assign the current rack and position
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- (10*(rack-1)) + positions[pos_index]
              
              # Check if the subject changes or if we've filled the current rack
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                rack <- rack + 1  # Move to the next rack
                pos_index <- 1  # Reset position index
              } else {
                pos_index <- pos_index + 1  # Move to the next position in the current rack
                
                # If all positions in the rack are filled, move to the next rack
                if (pos_index > length(positions)) {
                  rack <- rack + 1
                  pos_index <- 1  # Reset position index
                }
                
              }
              
            }
          }
          else if(input$new_rack_per_tissue == "Yes, 2 tissues per rack"){
            
            rack <- 3  # Initial rack number
            pos_index <- 1  # Index to track position in c(1, 3, 5, 7, 9)
            positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  # Allowed positions
            tissueCounter <- 0
            
            for (i in seq_len(total_tissues)) {
              # Assign the current rack and position
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- (10*(rack-1)) + positions[pos_index]
              
              if (i < total_tissues && subject_data$Tissue[i] != subject_data$Tissue[i + 1]) {
                tissueCounter <- tissueCounter + 1
              }
              
              # Check if the tissue changes
              if (i < total_tissues && tissueCounter > 1) {
                tissueCounter <- 0
                rack <- rack + 1  # Start a new rack when 2 tissues change
                pos_index <- 1  # Reset position for new rack
              } 
              else {
                pos_index <- pos_index + 1  # Move to the next position in the current rack
                
                # If all positions in the rack are filled, move to the next rack
                if (pos_index > length(positions)) {
                  rack <- rack + 1
                  pos_index <- 1  # Reset position index
                }
                
              }
              
            }
          }
          else if (input$new_rack_per_tissue == "No"){
            # Define the starting rack number
            starting_rack <- 2
            
            # Initialize Rack and Pos columns
            #subject_data$Rack <- NA
            #subject_data$Pos <- NA
            
            # Calculate the total number of tissues
            total_tissues <- nrow(subject_data)
            
            # Loop through all tissues and assign Rack and Pos values
            for (i in seq_len(total_tissues)) {
              # Determine the rack and position based on the index
              rack_offset <- (i - 1) %/% 10  # Increment rack every 10 tissues
              position_offset <- (i - 1) %% 10 + 1  # Offset within the rack (1 to 10)
              
              # Assign rack and position
              subject_data$Rack[i] <- starting_rack + rack_offset + 1
              subject_data$Pos[i] <- (starting_rack + rack_offset) * 10 + position_offset
            }
          }
        } 
        else if (input$group_by == "Subject"){
          
          #sort by subject
          subject_data <- subject_data[order(subject_data$animal_id, match(subject_data$Tissue, tissues)), ]
          
          if(input$new_rack_per_animal == "Yes"){
            
            rack <- 3  # Initial rack number
            pos_index <- 1  # Index to track position in c(1, 3, 5, 7, 9)
            positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  # Allowed positions
            
            for (i in seq_len(total_tissues)) {
              # Assign the current rack and position
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- (10*rack) + positions[pos_index]
              
              # Check if the subject changes or if we've filled the current rack
              if (i < total_tissues && subject_data$animal_id[i] != subject_data$animal_id[i + 1]) {
                rack <- rack + 1  # Move to the next rack
                pos_index <- 1  # Reset position index
              } else {
                pos_index <- pos_index + 1  # Move to the next position in the current rack
                
                # If all positions in the rack are filled, move to the next rack
                if (pos_index > length(positions)) {
                  rack <- rack + 1
                  pos_index <- 1  # Reset position index
                }
                
              }
              
            }
          }
          else if (input$new_rack_per_animal == "No"){
            # Define the starting rack number
            starting_rack <- 2
            
            # Initialize Rack and Pos columns
            #subject_data$Rack <- NA
            #subject_data$Pos <- NA
            
            # Calculate the total number of tissues
            total_tissues <- nrow(subject_data)
            
            # Loop through all tissues and assign Rack and Pos values
            for (i in seq_len(total_tissues)) {
              # Determine the rack and position based on the index
              rack_offset <- (i - 1) %/% 10  # Increment rack every 10 tissues
              position_offset <- (i - 1) %% 10 + 1  # Offset within the rack (1 to 10)
              
              # Assign rack and position
              subject_data$Rack[i] <- starting_rack + rack_offset + 1
              subject_data$Pos[i] <- (starting_rack + rack_offset) * 10 + position_offset
            }
          }
        }
        
        
        # Default rack and position assignment for other models
        #subject_data$Rack <- as.integer(3 + (seq_len(total_tissues) - 1) %/% 10)
        #subject_data$Pos <- as.integer((seq_len(total_tissues) - 1) %% 10 + 1)
      }
      
      ######Rack background adding
      if (input$rack_option == "Yes"){
        
        rack_rows <- list()
        total_tissues <- nrow(subject_data)
        
        if (input$gc_model == "2480") {
          rack <- 3
          position <- 1
          
          i <- 1  # Start from the first row
          while (i <= total_tissues) {
            subject_data$Rack[i] <- rack
            subject_data$Pos[i] <- position
            rack_rows <- append(rack_rows, list(subject_data[i, ]))
            
            if (subject_data$Pos[i] == 10) {
              # Handle the cascade when position == 10
              print("10!")
              
              # Remove the last row (current tissue)
              rack_rows <- rack_rows[-length(rack_rows)]
              
              # Insert `rack_background` row
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "rack_background",
                Rack = rack,
                Pos = 10,
                stringsAsFactors = FALSE
              )))
              
              # Insert `skip_rack` row in the next rack
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "skip_rack",
                Rack = rack + 1,  # Skip one rack
                Pos = 1,
                stringsAsFactors = FALSE
              )))
              
              # Update the current tissue row (moved to next rack)
              position <- 1  # Reset position for the new rack
              rack <- rack + 2  # Increment the rack number
              
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- position
              rack_rows <- append(rack_rows, list(subject_data[i, ]))
              
              # Increment remaining rows
              subject_data$Rack[(i + 1):total_tissues] <- subject_data$Rack[(i + 1):total_tissues] + 1
              subject_data$Pos[(i + 1):total_tissues] <- (subject_data$Pos[(i + 1):total_tissues] - 1) %% 10 + 1
              
            } else if (i < total_tissues && subject_data$Rack[i] != subject_data$Rack[i + 1]) {
              # Insert `rack_background` row for non-full racks
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "rack_background",
                Rack = rack,
                Pos = position + 1,  # Current position + 1
                stringsAsFactors = FALSE
              )))
              
              # Insert `skip_rack` row for the next rack
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "skip_rack",
                Rack = subject_data$Rack[i + 1],  # Next rack
                Pos = 1,
                stringsAsFactors = FALSE
              )))
              
              # Update remaining rows
              subject_data$Rack[(i + 1):total_tissues] <- subject_data$Rack[(i + 1):total_tissues] + 1
              
              # Move to the next rack
              rack <- rack + 2
              position <- 1
            } else {
              position <- position + 1
            }
            
            i <- i + 1  # Increment loop index
            total_tissues <- nrow(subject_data)  # Update total tissues after modification
          }
        }
        else if (input$gc_model == "1480"){
          
          
          
          rack <- 3
          position <- 1
          
          i <- 1  # Start from the first row
          subject_data$Pos <- (subject_data$Pos - 1) %% 10
          while (i <= total_tissues) {
            #print(subject_data$Rack[(i + 1):total_tissues])
            
            
            print(paste("rack:", max(subject_data$Rack, na.rm = TRUE)))
            
            # Find the maximum value in the 'rack' column
            max_rack <- max(subject_data$Rack, na.rm = TRUE)
            
            # Filter rows where the 'rack' column equals the maximum value
            max_rack_rows <- subject_data[subject_data$Rack == max_rack, ]
            
            # Print the rows
            print(max_rack_rows)
            
            
            # Assign the current rack and Pos following 1480 rules
            subject_data$Rack[i] <- rack
            subject_data$Pos[i] <- (10 * (rack-1)) + position  # 1480-specific Pos calculation
            rack_rows <- append(rack_rows, list(subject_data[i, ]))
            
            # Handle when the position reaches the last spot in the rack (10)
            if (position %% 10 == 0) {
              print("10!")
              
              # Remove the last row (current tissue)
              rack_rows <- rack_rows[-length(rack_rows)]
              
              # Insert `rack_background` row
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "rack_background",
                Rack = rack,
                Pos = (10 * (rack-1)) + 10,  # Last position in the current rack
                stringsAsFactors = FALSE
              )))
              
              # add a skip rack if it is not the last row
              if (i < total_tissues) {
                rack_rows <- append(rack_rows, list(data.frame(
                  animal_id = NA,
                  group = NA,
                  Timepoint = NA,
                  Tissue = "skip_rack",
                  Rack = rack + 1,  # Move to the next rack
                  Pos = (10 * rack) + 1,  # First position in the next rack
                  stringsAsFactors = FALSE
                )))
              }
              
              
              # Update the current tissue row (moved to the rack after the skip rack)
              rack <- rack + 2  # Increment the rack by 2
              position <- 1  # Reset position index for the new rack
              
              subject_data$Rack[i] <- rack
              subject_data$Pos[i] <- (10 * (rack-1)) + position
              
              rack_rows <- append(rack_rows, list(subject_data[i, ]))
              
              position <- position + 1 
              
              subject_data$Rack[i+1] <- rack
              subject_data$Pos[i+1] <- (10 * (rack-1)) + position
              
              rack_rows <- append(rack_rows, list(subject_data[i+1, ]))
              
              
              if (i < total_tissues) { #Update positions of all unassigned/not evaluated racks
                subject_data$Rack[(i + 1):total_tissues] <- subject_data$Rack[(i + 1):total_tissues] + (1) 
              }
              i <- i + 1
              position <- position + 1
              
              # Handle when the rack number changes but is not fulls
            } 
            else if (i < total_tissues && subject_data$Rack[i] %% 2 == 0) {
              print(paste("rack change:", subject_data$Rack[i], "next rack is:", subject_data$Rack[i+1]))
              # Insert `rack_background` row
              rack_rows <- append(rack_rows, list(data.frame(
                animal_id = NA,
                group = NA,
                Timepoint = NA,
                Tissue = "rack_background",
                Rack = rack,
                Pos = (10 * (rack - 1)) + position + 1,  # Explicitly use the next position
                stringsAsFactors = FALSE
              )))
              
              # Add `skip_rack` row only if there are more tissues
              if (i < total_tissues) {
                rack_rows <- append(rack_rows, list(data.frame(
                  animal_id = NA,
                  group = NA,
                  Timepoint = NA,
                  Tissue = "skip_rack",
                  Rack = rack + 1,  # Next rack
                  Pos = (10 * rack) + 1,  # First position in the next rack
                  stringsAsFactors = FALSE
                )))
              }
              
              # Update rack and reset position
              rack <- rack + 2
              position <- 1
              
              if (i < total_tissues) { #Update positions of all unassigned/not evaluated racks
                subject_data$Rack[(i):total_tissues] <- subject_data$Rack[(i):total_tissues] + 1
              }
              
            } else {
              # Move to the next position
              position <- position + 1
            }
            
            i <- i + 1  # Increment loop index
          }
          
        }
        
        updated_data <- do.call(rbind, rack_rows)
        subject_data <- updated_data
      }
    }
    
    return(subject_data)
  })
  
  # Render data table ----------------------------------------------------------
  output$preview_table <- renderDT({
    req(preview_ready())  # Wait until the flag is true
    
    model_rows <- createStds()  # Standard rows
    subject_data <- generate_data()  # User-generated data
    
    if (is.null(subject_data)) return(NULL)
    
    # Combine model rows and subject data, but don't sort model rows
    final_data <- rbind(model_rows, subject_data)
    
    # Replace NA with empty strings
    final_data[is.na(final_data)] <- ""
    
    # Separate model rows and subject data for sorting purposes
    model_data_rows <- final_data[final_data$Tissue %in% c("Standard-1", "Standard-2", "Standard-3", "BKG"), ]
    subject_data_rows <- final_data[!(final_data$Tissue %in% c("Standard-1", "Standard-2", "Standard-3", "BKG")), ]
    
    # Combine the model data (unsorted) with the already sorted subject data
    final_data <- rbind(model_data_rows, subject_data_rows)
    
    # Render the table
    datatable(
      final_data,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = "copy",
            text = "COPY",
            title = NULL,
            customize = JS("
          function (copy) {
            // Remove header row (first line of the copied text)
            var lines = copy.split('\\n');
            lines.splice(0, 1); // Remove the header
            return lines.join('\\n'); // Rejoin the remaining lines
          }
        ")
          ),
          'csv',
          'excel'
        ),
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  
}

# Run the application
run <- shinyApp(ui, server = server)
