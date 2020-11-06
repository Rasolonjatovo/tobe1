###################SERVER######################
req_packages <- c("shiny",
                  "usmap",
                  "ggplot2",
                  "dplyr",
                  "tidyr",
                  "tibble",
                  "DT",
                  "DBI",
                  "RPostgreSQL")
where_clause <- "WHERE"
lapply(req_packages, require, character.only = TRUE)
db <- 'postgres' 

host_db <- "database-2.c0gfscutioly.us-east-2.rds.amazonaws.com"

db_port <- 5432   # or any other port specified by the DBA

db_user <-  'postgres'

db_password <- 'lalaina30'
drv <- dbDriver("PostgreSQL")

if.is.empty <- function(x){
  is.null(need(x, message = FALSE))}


server <- function(input, output, session){
  #setup database connection
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  
  #create reactive values and define default data to display
  default_query <- "SELECT * FROM metadata_search"
  #default_query <- "SELECT * FROM load_from_s3.locations"
  #default_query1 <- "SELECT * FROM load_from_s3.docs"
  
  values <- reactiveValues(reports = dbGetQuery(con, default_query),
                           map_data = dbGetQuery(con, default_query)%>%
                             group_by(location_name) %>%
                             summarize(topic_location = n()) %>%
                             rename(state = location_name) %>%
                             filter(state != "" & state != "All") %>%
                             mutate(state = tolower(state)),
                           comp_reports_a = dbGetQuery(con, default_query),
                           comp_map_data_a = dbGetQuery(con, default_query)%>%
                             group_by(location_name) %>%
                             summarize(topic_location = n()) %>%
                             rename(state = location_name) %>%
                             filter(state != "" & state != "All") %>%
                             mutate(state = tolower(state)),
                           comp_reports_b = dbGetQuery(con, default_query),
                           comp_map_data_b = dbGetQuery(con, default_query)%>%
                             group_by(location_name) %>%
                             summarize(topic_location = n()) %>%
                             rename(state = location_name) %>%
                             filter(state != "" & state != "All") %>%
                             mutate(state = tolower(state))
  )
  dbDisconnect(con)
  #Response to button press on standard page
  
  observeEvent(input$submit_search, {
    con <- dbConnect(drv, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
    
    if(input$searchterms == ""){
      sql_query <- "SELECT * FROM metadata_search"
    }else{
      searchterms <- strsplit(input$searchterms, " ") %>% unlist()
      or_operators <- grep("OR", searchterms)
      or_clauses <- character()
      removal_vector <- integer()
      where_clause <- "WHERE"
      if(length(or_operators) !=0){
        for(or in 1:length(or_operators)){
          removal_vector <- append(removal_vector, c((or_operators[or]-1), or_operators[or], (or_operators[or]+1)))
          or_pair <- paste("(",searchterms[or_operators[or]-1], "|", searchterms[or_operators[or]+1], ")", sep = "")
          clause <- paste("(title ~* '", or_pair,
                          "' OR priority ~* '", or_pair,
                          "' OR location_name ~* '", or_pair,
                          "' OR author_name ~* '", or_pair,
                          "' OR hsec_sin ~* '", or_pair,
                          "' OR sbu ~* '", or_pair,
                          "')",
                          sep = "")
          or_clauses <- paste(or_clauses, clause, sep = " AND ")
        }
        searchterms <- searchterms[-removal_vector]
      }
      for(term in searchterms){
        clause <- paste("(title ~* '", term,
                        "' OR priority ~* '", term,
                        "' OR location_name ~* '", term,
                        "' OR author_name ~* '", term,
                        "' OR hsec_sin ~* '", term,
                        "' OR sbu ~* '", term,
                        "')",
                        sep = "")
        where_clause <- paste(where_clause, clause, sep = " AND ")
      }
      where_clause <-  where_clause %>%
        paste(or_clauses, sep = " ") %>%
        gsub(x = ., pattern = "WHERE[[:space:]]+AND", replacement = "WHERE")
      sql_query <- paste("SELECT * FROM metadata_search", where_clause, ";", sep = "")
    }
    
    tmp <- dbGetQuery(con, paste("SELECT * FROM metadata_search", where_clause, ";", sep = ""))
    if(nrow(tmp) == 0){ showModal(modalDialog(
      title = "No Results",
      "This search yielded no results. Please try a different search."))}else{
        tmp <- tmp %>%
          filter(product_date >= input$filter_date_range[1] & product_date <= input$filter_date_range[2])
        if(input$filter_fc_sin != "Select One"){
          tmp <- tmp %>%
            filter(fc_sin == tolower(input$filter_fc_sin))
        }
        if(input$filter_SAR != "Select One"){
          tmp <- tmp %>%
            filter(sar == tolower(input$filter_SAR) )
        }
        if(input$filter_PII != "Select One"){
          tmp <- tmp %>%
            filter(pii == tolower(input$filter_PII))
        }
        if(input$filter_foreign != "Select One"){
          tmp <- tmp %>%
            filter(foreign_release == tolower(input$filter_foreign))
        }
        if(input$filter_private != "Select One"){
          tmp <- tmp %>%
            filter(private_release == tolower(input$filter_private))
        }
        if(!is.null(input$filter_hsec_sin)){
          tmp <- tmp %>%
            filter(hsec_sin %in% input$filter_hsec_sin)
        }
        if(!is.null(input$filter_location)){
          tmp <- tmp %>%
            filter(location_name %in% input$filter_location)
        }
        if(!is.null(input$filter_priorities)){
          tmp <- tmp %>%
            filter(priority %in% input$filter_priorities)
        }
        if(!is.null(input$filter_sbu)){
          tmp <- tmp %>%
            filter(sbu %in% input$filter_sbu)
        }
        
        values$reports <- tmp
        values$map_data <- tmp %>%
          group_by(location_name) %>%
          summarize(topic_location = n()) %>%
          rename(state = location_name) %>%
          filter(state != "" & state != "All") %>%
          mutate(state = tolower(state))
      }
    dbDisconnect(con)
  })
  
  #Response to comp search a button press
  observeEvent(input$submit_comp_search_a,{
    con <- dbConnect(drv, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
    
    if(input$comp_search_a == ""){
      sql_query <- "SELECT * FROM metadata_search"
    }else{
      searchterms <- strsplit(input$comp_search_a, " ") %>% unlist()
      or_operators <- grep("OR", searchterms)
      or_clauses <- character()
      removal_vector <- integer()
      where_clause <- "WHERE"
      if(length(or_operators) !=0){
        for(or in 1:length(or_operators)){
          removal_vector <- append(removal_vector, c((or_operators[or]-1), or_operators[or], (or_operators[or]+1)))
          or_pair <- paste("(",searchterms[or_operators[or]-1], "|", searchterms[or_operators[or]+1], ")", sep = "")
          clause <- paste("(title ~* '", or_pair,
                          "' OR priority ~* '", or_pair,
                          "' OR location_name ~* '", or_pair,
                          "' OR author_name ~* '", or_pair,
                          "' OR hsec_sin ~* '", or_pair,
                          "' OR sbu ~* '", or_pair,
                          "')",
                          sep = "")
          or_clauses <- paste(or_clauses, clause, sep = " AND ")
        }
        searchterms <- searchterms[-removal_vector]
      }
      for(term in searchterms){
        clause <- paste("(title ~* '", term,
                        "' OR priority ~* '", term,
                        "' OR location_name ~* '", term,
                        "' OR author_name ~* '", term,
                        "' OR hsec_sin ~* '", term,
                        "' OR sbu ~* '", term,
                        "')",
                        sep = "")
        where_clause <- paste(where_clause, clause, sep = " AND ")
      }
      where_clause <- gsub("WHERE AND", "WHERE", where_clause) %>%
        paste(or_clauses, sep = " ")
      sql_query <- paste("SELECT * FROM metadata_search", where_clause, ";", sep = "")
    }
    #tmp <- dbGetQuery(con, sql_query)
    tmp <- dbGetQuery(con, paste("SELECT * FROM metadata_search", where_clause, ";", sep = ""))
    if(nrow(tmp) == 0){ showModal(modalDialog(
      title = "No Results",
      "This search yielded no results. Please try a different search."))}else{
        tmp <- tmp %>%
          filter(product_date >= input$filter_date_range_comp_a[1] & product_date <= input$filter_date_range_comp_a[2])
        if(input$filter_fc_sin_comp_a != "Select One"){
          tmp <- tmp %>%
            filter(fc_sin == tolower(input$filter_fc_sin_comp_a))
        }
        if(input$filter_SAR_comp_a != "Select One"){
          tmp <- tmp %>%
            filter(sar == tolower(input$filter_SAR_comp_a) )
        }
        if(input$filter_PII_comp_a != "Select One"){
          tmp <- tmp %>%
            filter(pii == tolower(input$filter_PII_comp_a))
        }
        if(input$filter_foreign_comp_a != "Select One"){
          tmp <- tmp %>%
            filter(foreign_release == tolower(input$filter_foreign_comp_a))
        }
        if(input$filter_private_comp_a != "Select One"){
          tmp <- tmp %>%
            filter(private_release == tolower(input$filter_private_comp_a))
        }
        if(!is.null(input$filter_hsec_sin_comp_a)){
          tmp <- tmp %>%
            filter(hsec_sin %in% input$filter_hsec_sin)
        }
        if(!is.null(input$filter_location_comp_a)){
          tmp <- tmp %>%
            filter(location_name %in% input$filter_location_comp_a)
        }
        if(!is.null(input$filter_priorities_comp_a)){
          tmp <- tmp %>%
            filter(priority %in% input$filter_priorities_comp_a)
        }
        if(!is.null(input$filter_sbu_comp_a)){
          tmp <- tmp %>%
            filter(sbu %in% input$filter_sbu_comp_a)
        }
        values$comp_reports_a <- tmp
        values$comp_map_data_a <- tmp %>%
          group_by(location_name) %>%
          summarize(topic_location = n()) %>%
          rename(state = location_name) %>%
          filter(state != "" & state != "All") %>%
          mutate(state = tolower(state))
      }
    dbDisconnect(con)
  })
  
  #Response to comp search b button press
  observeEvent(input$submit_comp_search_b, {
    con <- dbConnect(drv, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
    
    if(input$comp_search_b == ""){
      sql_query <- "SELECT * FROM metadata_search"
    }else{
      searchterms <- strsplit(input$comp_search_b, " ") %>% unlist()
      or_operators <- grep("OR", searchterms)
      or_clauses <- character()
      removal_vector <- integer()
      where_clause <- "WHERE"
      if(length(or_operators) !=0){
        for(or in 1:length(or_operators)){
          removal_vector <- append(removal_vector, c((or_operators[or]-1), or_operators[or], (or_operators[or]+1)))
          or_pair <- paste("(",searchterms[or_operators[or]-1], "|", searchterms[or_operators[or]+1], ")", sep = "")
          clause <- paste("(title ~* '", or_pair,
                          "' OR priority ~* '", or_pair,
                          "' OR location_name ~* '", or_pair,
                          "' OR author_name ~* '", or_pair,
                          "' OR hsec_sin ~* '", or_pair,
                          "' OR sbu ~* '", or_pair,
                          "')",
                          sep = "")
          or_clauses <- paste(or_clauses, clause, sep = " AND ")
        }
        searchterms <- searchterms[-removal_vector]
      }
      for(term in searchterms){
        clause <- paste("(title ~* '", term,
                        "' OR priority ~* '", term,
                        "' OR location_name ~* '", term,
                        "' OR author_name ~* '", term,
                        "' OR hsec_sin ~* '", term,
                        "' OR sbu ~* '", term,
                        "')",
                        sep = "")
        where_clause <- paste(where_clause, clause, sep = " AND ")
      }
      where_clause <- gsub("WHERE AND", "WHERE", where_clause) %>%
        paste(or_clauses, sep = " ")
      sql_query <- paste("SELECT * FROM metadata_search", where_clause, ";", sep = "")
    }
    #tmp <- dbGetQuery(con, sql_query)
    #tmp <- dbGetQuery(con, paste("SELECT * FROM load_from_s3.docs", where_clause, ";", sep = ""))
    if(nrow(tmp) == 0){ showModal(modalDialog(
      title = "No Results",
      "This search yielded no results. Please try a different search."))}else{
        tmp <- tmp %>%
          filter(product_date >= input$filter_date_range_comp_b[1] & product_date <= input$filter_date_range_comp_b[2])
        if(input$filter_fc_sin_comp_b != "Select One"){
          tmp <- tmp %>%
            filter(fc_sin == tolower(input$filter_fc_sin_comp_b))
        }
        if(input$filter_SAR_comp_b != "Select One"){
          tmp <- tmp %>%
            filter(sar == tolower(input$filter_SAR_comp_b) )
        }
        if(input$filter_PII_comp_b != "Select One"){
          tmp <- tmp %>%
            filter(pii == tolower(input$filter_PII_comp_b))
        }
        if(input$filter_foreign_comp_b != "Select One"){
          tmp <- tmp %>%
            filter(foreign_release == tolower(input$filter_foreign_comp_b))
        }
        if(input$filter_private_comp_b != "Select One"){
          tmp <- tmp %>%
            filter(private_release == tolower(input$filter_private_comp_b))
        }
        if(!is.null(input$filter_hsec_sin_comp_b)){
          tmp <- tmp %>%
            filter(hsec_sin %in% input$filter_hsec_sin_comp_b)
        }
        if(!is.null(input$filter_location_comp_b)){
          tmp <- tmp %>%
            filter(location_name %in% input$filter_location_comp_b)
        }
        if(!is.null(input$filter_priorities_comp_b)){
          tmp <- tmp %>%
            filter(priority %in% input$filter_priorities_comp_b)
        }
        if(!is.null(input$filter_sbu_comp_b)){
          tmp <- tmp %>%
            filter(sbu %in% input$filter_sbu_comp_b)
        }
        values$comp_reports_b <- tmp
        values$comp_map_data_b <- tmp %>%
          group_by(location_name) %>%
          summarize(topic_location = n()) %>%
          rename(state = location_name) %>%
          filter(state != "" & state != "All") %>%
          mutate(state = tolower(state))
      }
    dbDisconnect(con)
  })
  #create results tables to display
  output$hsin_table <- renderDataTable(values$reports,
                                       options = list(pageLength = 10,
                                                      bLengthChange=FALSE))
  output$comp_hsin_table_a <- renderDataTable(values$comp_reports_a,
                                              options = list(pageLength = 10,
                                                             bLengthChange=FALSE))
  output$comp_hsin_table_b <- renderDataTable(values$comp_reports_b,
                                              options = list(pageLength = 10,
                                                             bLengthChange=FALSE))
  #Export results tables
  #Standard
  output$export_hsin_table <- downloadHandler(filename = "HSIN_reports.csv",
                                              content = function(file){
                                                write.csv(values$reports, file, row.names = FALSE)
                                              })
  #Comp A
  output$export_comp_hsin_table_a <- downloadHandler(filename = "HSIN_reports.csv",
                                                     content = function(file){
                                                       write.csv(values$comp_reports_a, file, row.names = FALSE)
                                                     })
  #Comp B
  output$export_comp_hsin_table_b <- downloadHandler(filename = "HSIN_reports.csv",
                                                     content = function(file){
                                                       write.csv(values$comp_reports_b, file, row.names = FALSE)
                                                     })
  
  
  #Histogram of report publish dates
  #Standard
  output$distPlot <- renderPlot({
    ggplot(values$reports, aes(x = product_date))+
      geom_histogram(bins = 30 ,fill = "navy blue")+
      theme_bw()})
  #Comp A
  output$comp_dist_plot_a <- renderPlot({
    ggplot(values$comp_reports_a, aes(x = product_date))+
      geom_histogram(bins = 30 ,fill = "navy blue")+
      theme_bw()})
  #Comp B
  output$comp_dist_plot_b <- renderPlot({
    ggplot(values$comp_reports_b, aes(x = product_date))+
      geom_histogram(bins = 30 ,fill = "navy blue")+
      theme_bw()})
  
  #Choropleth map
  #Standard
  output$mapPlot <- renderPlot({
    plot_usmap(regions = "states",
               data = values$map_data,
               values = "topic_location") +
      labs(fill = "Topic Location")
  })
  #Comp A
  output$comp_map_plot_a <- renderPlot({
    plot_usmap(regions = "states",
               data = values$comp_map_data_a,
               values = "topic_location")+
      labs(fill = "Topic Location")
  })
  #Comp B
  output$comp_map_plot_b <- renderPlot({
    plot_usmap(regions = "states",
               data = values$comp_map_data_b,
               values = "topic_location")+
      labs(fill = "Topic Location")
  })
  
  #Compliance status barplot
  #Standard
  output$stackbarPlot <- renderPlot({
    #combine different boolean variables into one for ease of plotting
    pivot_longer(values$reports,
                 cols = c(sar,
                          p_crcl,
                          pii,
                          foreign_release,
                          private_release),
                 names_to = "compliance_variable",
                 values_to = "compliance_value") %>%
      ggplot(aes(x = compliance_variable, fill = compliance_value))+
      geom_bar(position = "stack") +
      coord_flip()
  })
  #Comp A
  output$comp_stackbar_plot_a <- renderPlot({
    pivot_longer(values$comp_reports_a,
                 cols = c(sar,
                          p_crcl,
                          pii,
                          foreign_release,
                          private_release),
                 names_to = "compliance_variable",
                 values_to = "compliance_value") %>%
      ggplot(aes(x = compliance_variable, fill = compliance_value))+
      geom_bar(position = "stack") +
      coord_flip()
  })
  #Comp B
  output$comp_stackbar_plot_b <- renderPlot({
    pivot_longer(values$comp_reports_b,
                 cols = c(sar,
                          p_crcl,
                          pii,
                          foreign_release,
                          private_release),
                 names_to = "compliance_variable",
                 values_to = "compliance_value") %>%
      ggplot(aes(x = compliance_variable, fill = compliance_value))+
      geom_bar(position = "stack") +
      coord_flip()
  })
  
  #General purpose clustered barplot
  #Standard
  output$clusterbarPlot <- renderPlot({
    ggplot(values$reports, aes(x = eval(as.name(input$cluster_x)),
                               y = eval(as.name(input$cluster_y)),
                               fill = eval(as.name(input$cluster_fill)))) +
      labs(x = input$cluster_x,
           y = input$cluster_y,
           fill = input$cluster_fill) +
      geom_col(position = "dodge")
  })
  #Comp A
  output$comp_clusterbar_plot_a <- renderPlot({
    ggplot(values$comp_reports_a, aes(x = eval(as.name(input$comp_cluster_x_a)),
                                      y = eval(as.name(input$comp_cluster_y_a)),
                                      fill = eval(as.name(input$comp_cluster_fill_a)))) +
      labs(x = input$cluster_x,
           y = input$cluster_y,
           fill = input$cluster_fill) +
      geom_col(position = "dodge")
  })
  #Comp B
  output$comp_clusterbar_plot_b <- renderPlot({
    ggplot(values$comp_reports_b, aes(x = eval(as.name(input$comp_cluster_x_b)),
                                      y = eval(as.name(input$comp_cluster_y_b)),
                                      fill = eval(as.name(input$comp_cluster_fill_b)))) +
      labs(x = input$cluster_x,
           y = input$cluster_y,
           fill = input$cluster_fill) +
      geom_col(position = "dodge")
  })
}

###################UI#################"

req_packages <- c("shiny",
                  "usmap",
                  "dplyr",
                  "tidyr",
                  "tibble",
                  "DT",
                  "ggplot2",
                  "DBI",
                  "RPostgreSQL")
lapply(req_packages, require, character.only = TRUE)
cluster_bar_names <- c("product_date",
                       "fc_sin",
                       "sar",
                       "pii",
                       "foreign_release",
                       "private_release",
                       "p_crcl",
                       "priority",
                       "location_name",
                       "author_name",
                       "hsec_sin",
                       "sbu")
ui=shinyUI(
  navbarPage("HSIN NavBar Prototype",
             tabPanel("Search",
                      column(6,
                             wellPanel(
                               textInput("searchterms", "Enter search terms here"),
                               #Filter selection
                               column(6,
                                      selectInput("filter_fc_sin", "FC Sin",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_SAR", "Suspicious Activity Reporting",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_PII", "Contains PII",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_foreign", "Releasable to Foreign persons",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_private", "Releaseable to Private Sector",
                                                  choices = c("Select One", "TRUE", "FALSE"))
                               ),
                               column(6,
                                      dateRangeInput("filter_date_range", "Report publication date",
                                                     start = "1900-01-01", end = Sys.Date()),
                                      selectInput("filter_hsec_sin", "HSEC SIN", multiple = TRUE,
                                                  choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                              "DISASTERS (HSEC-2)",
                                                              "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                              "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "ILLICIT DRUG OPS (HSEC-5)",
                                                              "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                              "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                              "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                              "FOREIGN NATION OPS (HSEC-7)",
                                                              "TERRORIST OPS (HSEC-8)",
                                                              "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                              "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                              "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_location", "Report Location", multiple = TRUE,
                                                  choices = c("Alabama",
                                                              "Alaska",
                                                              "Arizona",
                                                              "Arkansas",
                                                              "California",
                                                              "Colorado",
                                                              "Connecticut",
                                                              "Delaware",
                                                              "District of Columbia",
                                                              "Florida",
                                                              "Georgia",
                                                              "Hawaii",
                                                              "Idaho",
                                                              "Illinois",
                                                              "Indiana",
                                                              "Iowa",
                                                              "Kansas",
                                                              "Kentucky",
                                                              "Louisiana",
                                                              "Maine",
                                                              "Maryland",
                                                              "Massachusetts",
                                                              "Michigan",
                                                              "Minnesota",
                                                              "Mississippi",
                                                              "Missouri",
                                                              "Montana",
                                                              "Nebraska",
                                                              "Nevada",
                                                              "New Hampshire",
                                                              "New Jersey",
                                                              "New Mexico",
                                                              "New York",
                                                              "North Carolina",
                                                              "North Dakota",
                                                              "Ohio",
                                                              "Oklahoma",
                                                              "Oregon",
                                                              "Pennsylvania",
                                                              "Puerto Rico",
                                                              "Rhode Island",
                                                              "South Carolina",
                                                              "South Dakota",
                                                              "Tennessee",
                                                              "Texas",
                                                              "Utah",
                                                              "Vermont",
                                                              "Virgin Islands",
                                                              "Virginia",
                                                              "Washington",
                                                              "West Virginia",
                                                              "Wisconsin",
                                                              "Wyoming")
                                      ),
                                      selectInput("filter_priorities", "Priority", multiple = TRUE,
                                                  choices = c("Aviation Security",
                                                              "Aviation/Surface Transportation Security",
                                                              "Border Security",
                                                              "Border Security/Transnational Organized Crime",
                                                              "Countering Violent Extremism",
                                                              "Counterintelligence",
                                                              "Counterterrorism",
                                                              "Cyber Security",
                                                              "Mass Violence",
                                                              "Trade and Economic Security",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_sbu", "SBU Classification", multiple = TRUE,
                                                  choices = c("CII",
                                                              "FOUO",
                                                              "LES",
                                                              "Open Source",
                                                              "PCII",
                                                              "SSI",
                                                              "Unclassified",
                                                              "Not Applicable")
                                      )
                                      
                               ),
                               #Button to submit search terms
                               actionButton("submit_search", "Submit"))),
                      column(6,
                             tabsetPanel(type = "tabs",
                                         tabPanel("Time series", plotOutput("distPlot")),
                                         tabPanel("Compliance", plotOutput("stackbarPlot")),
                                         tabPanel("Map", plotOutput("mapPlot")),
                                         tabPanel("Cluster Bar", plotOutput("clusterbarPlot"),
                                                  column(4, selectInput("cluster_x",
                                                                        "x-axis",
                                                                        cluster_bar_names)),
                                                  column(4,selectInput("cluster_y",
                                                                       "y-axis",
                                                                       cluster_bar_names)),
                                                  column(4,selectInput("cluster_fill",
                                                                       "Subgroup",
                                                                       cluster_bar_names)))
                             )
                      ),
                      fluidRow(
                        column(12,
                               div(style= "overflow-x: scroll", dataTableOutput("hsin_table")),
                               downloadButton("export_hsin_table", "Export Data")
                        )
                      )
             ),
             tabPanel("Comparative Search",
                      column(6,
                             wellPanel(
                               textInput("comp_search_a", "Enter search terms here"),
                               actionButton("submit_comp_search_a", "Submit"),
                               column(6,
                                      
                                      selectInput("filter_fc_sin_comp_a", "FC Sin",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_SAR_comp_a", "Suspicious Activity Reporting",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_PII_comp_a", "Contains PII",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_foreign_comp_a", "Releasable to Foreign persons",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_private_comp_a", "Releaseable to Private Sector",
                                                  choices = c("Select One", "TRUE", "FALSE"))
                               ),
                               column(6,
                                      dateRangeInput("filter_date_range_comp_a", "Report publication date"),
                                      selectInput("filter_hsec_sin_comp_a", "HSEC SIN", multiple = TRUE,
                                                  choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                              "DISASTERS (HSEC-2)",
                                                              "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                              "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "ILLICIT DRUG OPS (HSEC-5)",
                                                              "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                              "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                              "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                              "FOREIGN NATION OPS (HSEC-7)",
                                                              "TERRORIST OPS (HSEC-8)",
                                                              "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                              "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                              "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_location_comp_a", "Report Location", multiple = TRUE,
                                                  choices = c("Alabama",
                                                              "Alaska",
                                                              "Arizona",
                                                              "Arkansas",
                                                              "California",
                                                              "Colorado",
                                                              "Connecticut",
                                                              "Delaware",
                                                              "District of Columbia",
                                                              "Florida",
                                                              "Georgia",
                                                              "Hawaii",
                                                              "Idaho",
                                                              "Illinois",
                                                              "Indiana",
                                                              "Iowa",
                                                              "Kansas",
                                                              "Kentucky",
                                                              "Louisiana",
                                                              "Maine",
                                                              "Maryland",
                                                              "Massachusetts",
                                                              "Michigan",
                                                              "Minnesota",
                                                              "Mississippi",
                                                              "Missouri",
                                                              "Montana",
                                                              "Nebraska",
                                                              "Nevada",
                                                              "New Hampshire",
                                                              "New Jersey",
                                                              "New Mexico",
                                                              "New York",
                                                              "North Carolina",
                                                              "North Dakota",
                                                              "Ohio",
                                                              "Oklahoma",
                                                              "Oregon",
                                                              "Pennsylvania",
                                                              "Puerto Rico",
                                                              "Rhode Island",
                                                              "South Carolina",
                                                              "South Dakota",
                                                              "Tennessee",
                                                              "Texas",
                                                              "Utah",
                                                              "Vermont",
                                                              "Virgin Islands",
                                                              "Virginia",
                                                              "Washington",
                                                              "West Virginia",
                                                              "Wisconsin",
                                                              "Wyoming")
                                      ),
                                      selectInput("filter_priorities_comp_a", "Priority", multiple = TRUE,
                                                  choices = c("Aviation Security",
                                                              "Aviation/Surface Transportation Security",
                                                              "Border Security",
                                                              "Border Security/Transnational Organized Crime",
                                                              "Countering Violent Extremism",
                                                              "Counterintelligence",
                                                              "Counterterrorism",
                                                              "Cyber Security",
                                                              "Mass Violence",
                                                              "Trade and Economic Security",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_sbu_comp_a", "SBU Classification", multiple = TRUE,
                                                  choices = c("CII",
                                                              "FOUO",
                                                              "LES",
                                                              "Open Source",
                                                              "PCII",
                                                              "SSI",
                                                              "Unclassified",
                                                              "Not Applicable")
                                      )
                                      
                               ),
                               br(),
                               tabsetPanel(type = "tabs",
                                           tabPanel("Time series", plotOutput("comp_dist_plot_a")),
                                           tabPanel("Compliance", plotOutput("comp_stackbar_plot_a")),
                                           tabPanel("Map", plotOutput("comp_map_plot_a")),
                                           tabPanel("Cluster Bar", plotOutput("comp_clusterbar_plot_a"),
                                                    column(4, selectInput("comp_cluster_x_a",
                                                                          "x-axis",
                                                                          cluster_bar_names)),
                                                    column(4,selectInput("comp_cluster_y_a",
                                                                         "y-axis",
                                                                         cluster_bar_names)),
                                                    column(4,selectInput("comp_cluster_fill_a",
                                                                         "Subgroup",
                                                                         cluster_bar_names)))
                               )
                             ),
                             fluidRow(
                               column(12,
                                      div(style= "overflow-x: scroll", dataTableOutput("comp_hsin_table_a")),
                                      downloadButton("export_comp_hsin_table_a", "Export Data")
                               )
                             )
                      ),
                      column(6,
                             wellPanel(
                               textInput("comp_search_b", "Enter search terms here"),
                               actionButton("submit_comp_search_b", "Submit"),
                               column(6,
                                      selectInput("filter_fc_sin_comp_b", "FC Sin",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_SAR_comp_b", "Suspicious Activity Reporting",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_PII_comp_b", "Contains PII",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_foreign_comp_b", "Releasable to Foreign persons",
                                                  choices = c("Select One", "TRUE", "FALSE")),
                                      selectInput("filter_private_comp_b", "Releasable to Private Sector",
                                                  choices = c("Select One", "TRUE", "FALSE"))
                               ),
                               column(6,
                                      dateRangeInput("filter_date_range_comp_b", "Report publication date"),
                                      selectInput("filter_hsec_sin_comp_b", "HSEC SIN", multiple = TRUE,
                                                  choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                              "DISASTERS (HSEC-2)",
                                                              "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                              "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                              "ILLICIT DRUG OPS (HSEC-5)",
                                                              "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                              "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                              "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                              "FOREIGN NATION OPS (HSEC-7)",
                                                              "TERRORIST OPS (HSEC-8)",
                                                              "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                              "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                              "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_location_comp_b", "Report Location", multiple = TRUE,
                                                  choices = c("Alabama",
                                                              "Alaska",
                                                              "Arizona",
                                                              "Arkansas",
                                                              "California",
                                                              "Colorado",
                                                              "Connecticut",
                                                              "Delaware",
                                                              "District of Columbia",
                                                              "Florida",
                                                              "Georgia",
                                                              "Hawaii",
                                                              "Idaho",
                                                              "Illinois",
                                                              "Indiana",
                                                              "Iowa",
                                                              "Kansas",
                                                              "Kentucky",
                                                              "Louisiana",
                                                              "Maine",
                                                              "Maryland",
                                                              "Massachusetts",
                                                              "Michigan",
                                                              "Minnesota",
                                                              "Mississippi",
                                                              "Missouri",
                                                              "Montana",
                                                              "Nebraska",
                                                              "Nevada",
                                                              "New Hampshire",
                                                              "New Jersey",
                                                              "New Mexico",
                                                              "New York",
                                                              "North Carolina",
                                                              "North Dakota",
                                                              "Ohio",
                                                              "Oklahoma",
                                                              "Oregon",
                                                              "Pennsylvania",
                                                              "Puerto Rico",
                                                              "Rhode Island",
                                                              "South Carolina",
                                                              "South Dakota",
                                                              "Tennessee",
                                                              "Texas",
                                                              "Utah",
                                                              "Vermont",
                                                              "Virgin Islands",
                                                              "Virginia",
                                                              "Washington",
                                                              "West Virginia",
                                                              "Wisconsin",
                                                              "Wyoming")
                                      ),
                                      selectInput("filter_priorities_comp_b", "Priority", multiple = TRUE,
                                                  choices = c("Aviation Security",
                                                              "Aviation/Surface Transportation Security",
                                                              "Border Security",
                                                              "Border Security/Transnational Organized Crime",
                                                              "Countering Violent Extremism",
                                                              "Counterintelligence",
                                                              "Counterterrorism",
                                                              "Cyber Security",
                                                              "Mass Violence",
                                                              "Trade and Economic Security",
                                                              "Not Applicable")
                                      ),
                                      selectInput("filter_sbu_comp_b", "SBU Classification", multiple = TRUE,
                                                  choices = c("CII",
                                                              "FOUO",
                                                              "LES",
                                                              "Open Source",
                                                              "PCII",
                                                              "SSI",
                                                              "Unclassified",
                                                              "Not Applicable")
                                      )
                                      
                               ),
                               br(),
                               tabsetPanel(type = "tabs",
                                           tabPanel("Time series", plotOutput("comp_dist_plot_b")),
                                           tabPanel("Compliance", plotOutput("comp_stackbar_plot_b")),
                                           tabPanel("Map", plotOutput("comp_map_plot_b")),
                                           tabPanel("Cluster Bar", plotOutput("comp_clusterbar_plot_b"),
                                                    column(4, selectInput("comp_cluster_x_b",
                                                                          "x-axis",
                                                                          cluster_bar_names)),
                                                    column(4,selectInput("comp_cluster_y_b",
                                                                         "y-axis",
                                                                         cluster_bar_names)),
                                                    column(4,selectInput("comp_cluster_fill_b",
                                                                         "Subgroup",
                                                                         cluster_bar_names)))
                               )
                             ),
                             fluidRow(
                               column(12,
                                      div(style= "overflow-x: scroll", dataTableOutput("comp_hsin_table_b")),
                                      downloadButton("export_comp_hsin_table_b", "Export Data")
                               )
                             )
                      )
             )
  )
  
)
shinyApp(ui = ui, server = server)