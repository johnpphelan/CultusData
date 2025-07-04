table_choices <- c("Scale aging", "Scale Column Name Key", "Acoustic tag data", "Electrofishing", "Nest data", "Angling Basok",
                   "Angling derby", "Recapture data", "Scale 500", "Sweltzer creek", 
                   "Creel - Survey Questions", "Creel - Survey Responses",
                   "High reward tags", "High reward tags - claimed"
                   ,"Temperature - Shore", "Temperature - CLASS", "Tag Event",
                   "Tag Sold Information", "Angling derby - pumpkinseed",
                   "Creel - Main", "Creel - Fishing Results", "Creel - Demography", "Creel - ICE")
table_choices<-sort(table_choices)

creel_tables<-c("Creel - Main", "Creel - Fishing Results", "Creel - Demography", "Creel - ICE",
                "Creel - Survey Questions", "Creel - Survey Responses")
creel_tables<-sort(creel_tables)

table_col_search<-c("NA","pitTagNo", "transmitter", "receiver", "stationName", "receiver_name", "location", "acousticTagNo", "length", "weight", "tagID", "ScaleBookNo")
table_col_search <- sort(table_col_search)


template_list<-c("Abundance estiamte" = "", "angling Basok and Buck", "Angling Derby", "High Reward Tags", "High Reward Tags Claimed",
                 "Nest Data", "scale 500", "Scale Aging", "Spring Marking", "Sweltzer Creek", "Tag Data", "Scale Column Name Key")


source("scripts/f_metadata.R")

#basic theme
my_theme <- bslib::bs_theme(
  bootswatch = 'lumen',
  heading_font = 'Merriweather,serif',
  base_font = 'Source Sans Pro',
  "sidebar-bg" = '#ADD8E7'
)



ui <- page_sidebar(
  
  # add a css here and make the styling percentage based
  # 
  includeCSS("www/style.css"),
  
  title = tags$div(
    class = "page-title-bar",
    tags$span("Cultus Lake - Data", class = "title-text"),
    tags$img(
      src = "images/Smallmouth_bass.png",
      style = "margin-right: 10px; padding-left: 10px; height: 55px;"
    ),
    div(
      uiOutput("logout_ui"),
      class = "logout-button"
    )
  ),
  
  sidebar = sidebar(
    class = "sidebar",
    div(
      div(
        pickerInput(
          inputId = "table_name",
          label = "Select table",
          selected = 'Nest data',
          choices = c(table_choices),
          multiple = FALSE,
          options = pickerOptions(liveSearch = TRUE)
        ),
        class = "picker-input"
      ),
      div(
        uiOutput('date_filter'),
        class = "date-filter"    
      ),
      uiOutput("dynamicFilter"),
      
    )
  ),
  theme = my_theme,
  
  tags$script(HTML('
  $(document).ready(function(){
    var tooltipTriggerList = [].slice.call(document.querySelectorAll("[data-bs-toggle=\'tooltip\']"));
    var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl);
    });
  });
')),
  
  
  card(
    tabsetPanel(
      
      tabPanel("Selected Table",
               downloadButton(
                 class = "download-button",
                 "download_xlsx",
                 "Download file (.xlsx)"),
               div(class = "main-table",
                   DT::DTOutput("queried_table")
               )
      ),
      tabPanel("Selected Table - Plots",
               div(class = "axis-row",
                   div(class = "x-axis-select",
                       pickerInput(
                         inputId = "plot_x", 
                         label = "Select x-axis variable",
                         choices = NULL,
                         multiple = FALSE,
                         options = pickerOptions(liveSearch = TRUE)
                       ),
                   ),
                   div(class = "y-axis-select",
                       pickerInput(
                         inputId = "plot_y", 
                         label = "Select y-axis variable",
                         choices = NULL,
                         multiple = FALSE,
                         options = pickerOptions(liveSearch = TRUE)
                       )),
                   div(class = "radio-select",
                       radioButtons(
                         inputId = "plot_type",
                         label = "Plot type",
                         choices = c("Scatter" = "scatter", 
                                     "Line" = "line", 
                                     "Boxplot" = "boxplot"),
                         selected = "scatter",
                         inline = TRUE  # This puts the buttons in a row
                       ))
               ),
               card(
                 div(class = "plot-output",
                     plotly::plotlyOutput(outputId = "main_plot", height = "400px")
                 )),
               
      ),
      
      # New Tab for displaying the column names from the found tables
      tabPanel("Search By Column",
               div(
                 class = "column-search-header",
                 p("This finds the selected column name across")
               ),
               downloadButton(
                 class = "download-button",
                 "download_filtered_tables",
                 "Download Tables (.zip)"
               ),
               pickerInput(
                 inputId = "column_name",
                 label = "Select column to search",
                 selected = "NA",
                 choices = c(table_col_search),
                 multiple = FALSE,
                 options = pickerOptions((liveSearch = TRUE))
               ),
               
               uiOutput("columns_in_selected_tables")
               
               
      ),
      
      # How will this look?
      # We should have a drop down menu in the panel, where we can choose the creel tables
      # There are multiple choices
      tabPanel("Creel - Table Joins",
               
               # gets the names of the tables
               
               selectizeInput(
                 inputId = "Creel_selections",
                 label = "Select Creel tables to join",
                 choices = creel_tables,
                 multiple = TRUE,
                 options =list(create = TRUE)
               ),
               downloadButton(
                 class = "download-button",
                 "download_creel_tables",
                 "Download Creel Tables (.xlsx)"
               ),
               #we need to join the tables in the server, then display them!
               div(class = "main-table", DT::DTOutput("creel_joined_tables")),
               
               uiOutput("selected_creel_tables")
      ),
      
      tabPanel("Metadata",
               div(
                 class = "metadata-header",
                 tagList(
                   p("Each tab contains a list and descriptor for each column in the current tables."),
                   p("The unique key(s) for each table is listed."),
                   p("Hover over the column names to find a description for that specific column.")
                 )
               ),
               tabsetPanel(
                 tabPanel("Creel - Shifts",
                          creelMetadataBox(
                            title = "Creel - Shifts",
                            keys = "date, survey number",
                            fields = c(
                              "Date of the survey" = "Date",
                              "Unique ID for the survey" = "Survey number",
                              "Person conducting the survey" = "Surveyor",
                              "Time the shift started" = "Shift start time",
                              "Time the shift ended" = "Shift end time",
                              "Total hours worked during the shift" = "Hours worked"
                            )
                          )
                 ),
                 tabPanel("Creel - Fish Details",
                          creelMetadataBox(
                            title = "Creel - Fish Details",
                            keys = "date, survey number, Fish ID",
                            fields = c(
                              "Unique ID for the record" = "Survey number",
                              "Date of record" = "Date",
                              "Time of record" = "Time",
                              "Fish number caught on the current date" = "Fish No",
                              "Species of the fish caught" = "Spp",
                              "Length of fish in mm" = "length",
                              "Weight of fish in g" = "weight",
                              "Pit tag number, if present" = "PitTagNo",
                              "Notes/comments from operator" = "notes",
                              "Unique fish ID, independent of date" = "fishID",
                              "Acoustic tag number, if present" = "acousticTagNo"
                            )
                          )
                 ),
                 
                 tabPanel("Creel - Fish Results",
                          creelMetadataBox(
                            title = "Creel - Fish Results",
                            keys = "date, survey number",
                            fields = c(
                              "Unique ID for the record" = "Survey number",
                              "Date of record" = "Date",
                              "Time of record" = "Time",
                              "Total number of fish caught (may be released)" = "totCaught",
                              "Total number of fish retained" = "totRetained",
                              "Total number of small mouth bass caught" = "noSMBc",
                              "Total number of sockeye salmon(?) caught" = "NoKOc",
                              "Total number of cutthroat trout caught" = "NoCTc",
                              "Total number of rainbow trout caught" = "NoRBc",
                              "Total number of bull trout caught" = "NoBTc",
                              "Total number of lake trout caught" = "NoLTc",
                              "Total number of other species caught" = "NoOtherSppc",
                              "Total number of small mouth bass retained" = "noSMBr",
                              "Total number of sockeye salmon(?) retained" = "NoKOr",
                              "Total number of cutthroat trout retained" = "NoCTr",
                              "Total number of rainbow trout retained" = "NoRBr",
                              "Total number of bull trout retained" = "NoBTr",
                              "Total number of lake trout retained" = "NoLTr",
                              "Total number of other species retained" = "NoOtherSppr",
                              "Why did the angler release any of the fish caught" = "releaseReason",
                              "Hours fished per person" = "personHours",
                              "Number of anglers in the group" = "noAnglers",
                              "Number of rods being used" = "noRods",
                              "Where were the anglers fishing (dock or boat etc)" = "vessl",
                              "Preferred species for angling" = "preferredSpp",
                              "Where was the survey conducted" = "site"
                            )
                          )
                 ),
                 
                 tabPanel("Creel - Fisher Survey",
                          creelMetadataBox(
                            title = "Creel - Fisher Survey",
                            keys = "date, survey number, anglerID",
                            fields = c(
                              "Date of the survey" = "Date",
                              "Unique ID for the survey" = "Survey number",
                              "Time of survey" = "time",
                              "Reported gender" = "gender",
                              "Age category of participant" = "AgeClass",
                              "Type of fishing license held" = "licensePeriod",
                              "Resident of Canada?" = "residency",
                              "Where the participant is from" = "cityProvienceCountry",
                              "Post code" = "Post Code",
                              "Other information provided" = "notes",
                              "Unique ID for angler" = "AnglerID"
                            )
                          )
                 ),
               )
      ),
      
      
      # tabPanel("Download template forms",
      #          div(
      #            class = "under-construction",
      #            p("Under construction!"),
      #          ),
      #          card(
      #            selectizeInput(
      #              inputId = "template_selection",
      #              label = "Select template to download",
      #              choices = template_list,
      #              multiple = TRUE,
      #              options =list(create = TRUE)
      #            ),
      #          ),
      #          downloadButton(
      #            class = "download-button",
      #            "download_template",
      #            "Download template (.csv)"
      #          )
      # ),
      
      tabPanel("Contact",
               card(
                 a(
                   actionButton(inputId = "email1", label = "Contact Admin",
                                icon = icon("envelope", lib = "font-awesome")),
                   href = "mailto:john.phelan@gov.bc.ca"
                 )
               )
      )
      
      
    )
  ),
)

