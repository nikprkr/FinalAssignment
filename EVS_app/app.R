#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(bslib)
library(broom)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("European Values Survey- Selected Results"),
  selectInput("country_list", label = "Country", choices = country_list),
  selectInput("outcome",
              "Outcome Variables",
              choices = list("Mother working" = 1,
                             "Jobs to Nationals" = 2)),
  selectInput("age_polynomials",
              "Age Polynomials",
              choices = list("One" = 1,
                             "Two" = 2,
                             "Three"= 3,
                             "Four" = 4)),
  navlistPanel(
    downloadButton("report", "Generate report"),
    tabPanel("Overview", "This project explores the degree to which citizens of various European countries agree with the following statements: 'When a mother works outside the home, the children suffer, and 'employers should give priority to nationals over immigrants when jobs are scarce.' The data used in this analysis was generated as part of the 2017 European Value Study (EVS). The EVS is described as 'a large-scale, cross-national and longitudinal survey research program on how Europeans think about family, work, religion, politics, and society. Repeated every nine years in an increasing number of countries, the survey provides insights into the ideas, beliefs, preferences, attitudes, values, and opinions of citizens all over Europe.'"),
    tabPanel("Exploration", 
             plotOutput("plot_mother"),
             plotOutput("plot_jobs"),
             plotOutput("plot_age_jobs"),
             plotOutput("plot_age_mother"),
             plotOutput("plot_sex_jobs"),
             plotOutput("plot_sex_mother"),
             plotOutput("plot_education_jobs"),
             plotOutput("plot_education_mother")),
    tabPanel("Regression", 
             tableOutput("reg_table_mother"),
             tableOutput("reg_table_jobs"),
             plotOutput("table"))
  )
)


library(shiny)
library(ggplot2) # load ggplot
library(dplyr)


server <- function(input, output) {
  thematic::thematic_shiny()
  
  data_country <- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country==input$country_list)
    }
    
    else { 
      data_clean
      
  }})
  
  
    mother<- reactive({
      if(input$country_list!="Aggregate") {
        data_clean %>%
        filter(country ==input$country_list) %>%
        filter(suffering_mother >0) %>%
        select(suffering_mother, country)
    }

    else { 
      data_clean %>%
      filter(suffering_mother>0) %>%
      select(suffering_mother, country)
    
  }})
  
  jobs<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country ==input$country_list) %>%
      filter(jobs_scarce>0) %>%
      select(jobs_scarce, country)
    }
    
    else { 
      data_clean %>%
        filter(jobs_scarce>0) %>%
        select(jobs_scarce, country)
      
    }})
  
  age_mother<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country ==input$country_list) %>%
      select(age,suffering_mother)%>%
      group_by(age) %>%
      summarise(mean_age_suffering = mean(suffering_mother)) %>%
      filter(age>0) %>%
      pivot_longer(
        cols = "mean_age_suffering",
        values_to = "values")
    }
    
    else { 
      data_clean %>%
        select(age,suffering_mother)%>%
        group_by(age) %>%
        summarise(mean_age_suffering = mean(suffering_mother)) %>%
        filter(age>0) %>%
        pivot_longer(
          cols = "mean_age_suffering",
          values_to = "values")
  }}) 
  
  age_jobs<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country ==input$country_list) %>%
      select(age,jobs_scarce)%>%
      group_by(age) %>%
      summarise(mean_age_jobs = mean(jobs_scarce)) %>%
      filter(age>0) %>%
      pivot_longer(
        cols = "mean_age_jobs",
        values_to = "values")
    }
    
    else { 
      data_clean %>%
        select(age,jobs_scarce)%>%
        group_by(age) %>%
        summarise(mean_age_jobs = mean(jobs_scarce)) %>%
        filter(age>0) %>%
        pivot_longer(
          cols = "mean_age_jobs",
          values_to = "values")
  }})
  
  sex_mother<- reactive({ 
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country==input$country_list) %>%
      group_by(sex) %>%
      summarise(mean_sex_suffering = mean(suffering_mother))%>%
      mutate(sex=as.character(sex))%>%
      mutate(sex=replace(sex, sex=="2","Female")) %>%
      mutate(sex=replace(sex, sex=="1","Male"))
    }
    
    else { 
      data_clean %>%
        group_by(sex) %>%
        summarise(mean_sex_suffering = mean(suffering_mother))%>%
        mutate(sex=as.character(sex))%>%
        mutate(sex=replace(sex, sex=="2","Female")) %>%
        mutate(sex=replace(sex, sex=="1","Male"))
  }})
  
  sex_jobs<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country==input$country_list) %>%
      group_by(sex) %>%
      summarise(mean_sex_jobs = mean(jobs_scarce)) %>%
      mutate(sex=as.character(sex))%>%
      mutate(sex=replace(sex, sex=="2","Female")) %>%
      mutate(sex=replace(sex, sex=="1","Male"))
    }
    
    else { 
      data_clean %>%
        group_by(sex) %>%
        summarise(mean_sex_jobs = mean(jobs_scarce)) %>%
        mutate(sex=as.character(sex))%>%
        mutate(sex=replace(sex, sex=="2","Female")) %>%
        mutate(sex=replace(sex, sex=="1","Male"))
  }})
  
  education_mother<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country==input$country_list) %>%
      filter(education>0 & education!=66) %>%
      group_by(education) %>%
      summarise(mean_education_suffering = mean(suffering_mother)) %>%
      mutate(education=as.character(education)) %>%
      mutate(education=replace(education, education=="2","Secondary")) %>%
      mutate(education=replace(education, education=="1","Primary")) %>%
      mutate(education=replace(education, education=="3","College"))
    }
    
    else { 
      data_clean %>%
        filter(education>0 & education!=66) %>%
        group_by(education) %>%
        summarise(mean_education_suffering = mean(suffering_mother)) %>%
        mutate(education=as.character(education)) %>%
        mutate(education=replace(education, education=="2","Secondary")) %>%
        mutate(education=replace(education, education=="1","Primary")) %>%
        mutate(education=replace(education, education=="3","College"))
  }})
  
  education_jobs<- reactive({
    if(input$country_list!="Aggregate") {
      data_clean %>%
      filter(country==input$country_list) %>%
      filter(education>0 & education!=66) %>%
      group_by(education) %>%
      summarise(mean_education_jobs = mean(jobs_scarce)) %>%
      mutate(education=as.character(education))%>%
      mutate(education=replace(education, education=="2","Secondary")) %>%
      mutate(education=replace(education, education=="1","Primary")) %>%
      mutate(education=replace(education, education=="3","College"))
    }
    
    else { 
      data_clean %>%
        filter(education>0 & education!=66) %>%
        group_by(education) %>%
        summarise(mean_education_jobs = mean(jobs_scarce)) %>%
        mutate(education=as.character(education))%>%
        mutate(education=replace(education, education=="2","Secondary")) %>%
        mutate(education=replace(education, education=="1","Primary")) %>%
        mutate(education=replace(education, education=="3","College"))
  }})
  
  
  # Generate Plots
  
  output$plot_mother <- renderPlot({
    
    ggplot(mother(), aes(x="suffering_mother"))+
      theme_minimal()+
      geom_bar(aes(x = suffering_mother)) +
      xlab("count") +
      ggtitle("Response Counts: Children Suffer When Mother Works")
  })
  
  output$plot_jobs <- renderPlot({
    
    ggplot(jobs(), aes(x="jobs_scarce"))+
      theme_minimal()+
      geom_bar(aes(x = jobs_scarce)) +
      xlab("count") +
      ggtitle("Response Counts: Jobs should be given to Nationals")
  })
  
  output$plot_age_mother <- renderPlot({
    
    ggplot(age_mother(), aes(age,values))+
      theme_minimal()+
      geom_point() +
      xlab("age") +
      ylab("mean response value")+
      ggtitle("Age and mean response to 'mother working' question")+
      geom_smooth(method=lm) 
  })
  
  output$plot_age_jobs <- renderPlot({
    
    # Render a barplot for mean age, sex, and education for mothers working
    ggplot(age_jobs(), aes(age,values))+
      theme_minimal()+
      geom_point() +
      xlab("age") +
      ylab("mean response value")+
      ggtitle("Age and mean response to 'jobs scarce' question")+
      geom_smooth(method=lm) 
  })
  
  output$plot_sex_jobs <- renderPlot({
    
    # Render a barplot for mean age, sex, and education for mothers working
    ggplot(data = sex_jobs(), aes(y=sex, x=mean_sex_jobs))+
      theme_minimal()+
      geom_bar(stat="identity") +
      xlab("mean response") +
      ylab("sex")+
      ggtitle("Sex and mean response to jobs question")
    
  })
  
  output$plot_sex_mother <- renderPlot({
    
    ggplot(sex_mother(), aes(y=sex, x=mean_sex_suffering))+
      theme_minimal()+
      geom_bar(stat = "identity") +
      xlab("mean response") +
      ylab("sex")+
      ggtitle("Sex and mean response to 'mother working' question")
    
  })
  
  output$plot_education_mother <- renderPlot({
    
    ggplot(education_mother(), aes(y=education, x=mean_education_suffering))+
      theme_minimal()+
      geom_bar(stat = "identity") +
      xlab("mean response") +
      ylab("educational attainment")+
      ggtitle("Educational attainment and response to 'children suffering' question")
    
  })
  
  output$plot_education_jobs <- renderPlot({
    
    ggplot(education_jobs(), aes(y=education, x=mean_education_jobs))+
      theme_minimal()+
      geom_bar(stat = "identity") +
      xlab("mean response") +
      ylab("educational attainment")+
      ggtitle("Educational attainment and response to 'children suffering' question")
    
  })
  
  suffering.mother.lm<- reactive({ lm(suffering_mother ~ poly(age,input$age_polynomials), data = data_country()) %>%
      tidy()
    
  })
  
  jobs.scarce.lm<- reactive({ lm(jobs_scarce ~ poly(age,input$age_polynomials), data = data_country()) %>%
      tidy()
    
  })
  
  # jobs_scarce_plot <- plot(jobs.scarce.lm)
  
  
  # observeEvent(input$outcome,{
  #     updateTabsetPanel(session, "params", selected = input$outcome)
  # })
  
  output$reg_table_mother <-  renderTable ({ suffering.mother.lm() })
  
  output$reg_table_jobs <-  renderTable ({ jobs.scarce.lm() })
  
  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "tempReport.Rmd")
        file.copy("tempReport.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(nms = input$country_list)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
}
# Run the application 
shinyApp(ui = ui, server = server)
