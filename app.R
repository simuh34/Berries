library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)


ber <- read.csv("berries.csv")

## look at number of unique values in each column 
a_1 <- ber %>% summarize_all(n_distinct)

## delete some column with constant values
ber %<>% select( - c(Program, Week.Ending, Geo.Level, Ag.District, Ag.District.Code, County, County.ANSI, Zip.Code, Region, watershed_code, Watershed, CV.... ))

b_3 <- ber %>% filter(Commodity=="STRAWBERRIES")

## reorder the data by state
b_3 <- b_3[order(b_3$State.ANSI),]

## delete some values that cannot be classified
b_3 %<>% filter(Domain.Category != "NOT SPECIFIED")
b_3 %<>% filter(Period == "YEAR")


## revise some columns and remove some duplicate information which can make the data more intuitive and make it easier for us to use the values in the following steps
b_3 %<>% separate(Data.Item, c("d1","d2"), sep="-")

unique(b_3$d1)
unique(b_3$d2)
b_3 %<>% select(-d1)

b_3 %<>% separate(d2, c("b1","b2"), sep=",")
unique(b_3$b1)
unique(b_3$b2)

b_3[is.na(b_3)] <- " "
for(i in 1:length(b_3$Year)){
  b_3$`Domain.Category`[i]<-str_replace(b_3$`Domain.Category`[i],"NOT SPECIFIED","NOT SPECIFIED, ")
}
b_3 %<>%  separate(Domain.Category, c("D1", "Domain2"), sep = ":")
b_3 %<>% select(-D1)

b_3 %<>% separate(Domain, c("Domain1_1","Domain1"), sep=",")
unique(b_3$Domain1_1)
unique(b_3$Domain1)
b_3 %<>% select(-Domain1_1)



## remove the NA
b_3 %<>% na.omit(b_3)
b_3$Value <-  as.numeric(as.numeric(b_3$Value))

## check 
summary(b_3)


## take the values and put them in separate columns
aa <- str_extract(b_3$Domain2, "[0-9].*$")
aa <- as.numeric(str_replace_all(aa, "[[:punct:]]", " "))

bb <- str_split(b_3$Domain2, "[0-9].*$")
bb <- unlist(bb)
bb <- str_trim(bb)
bb <- bb[-which(bb=="")]
bb <-str_replace_all(bb, "[[:punct:]]", " ")

## add the new column
straw1 <- b_3 %>% mutate(Domain = bb, Domain_value = aa)
straw1 %<>% select(-Domain2)
straw2  <-  na.omit(straw1)

s2 <- straw2 %>% group_by(State) %>% summarize(total=sum(Value)) 
s3 <- straw2 %>% group_by(Year, State) %>% summarize(total=sum(Value)) 



###ui part
ui <- fluidPage(
  titlePanel("strawberry"),
  
  tabsetPanel(
    tabPanel("Data Display",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Data",
                             label ="Choose Data:",
                             
                             choices = c("raw data",
                                         "cleaned data"))
                 
                 
               ),
               mainPanel(
                 
                 DT::dataTableOutput("table1")
               ) 
               
             )
    ),
    
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Var", 
                             label = "Choose result:",
                             
                             choices = c("total_value",
                                         "total_year_state"))
                           
                 
               ),
               
               mainPanel(
              
                 plotOutput("selectVar")
                
               ) 
               
             )
    )
  )
  
)

server <- function(input, output){
  
  
  # formulaText <- reactive({
  #   switch(input$Var,
  #          "total_value" = "Vbarplot of the total of each state",
  #          "total_year_state" = "line plot of total amount per year in each country")
  # })
  
  
  
  
  
  
  datachoose <- reactive({
    switch (input$Data,
            "raw data" = b_3,
            "cleaned data" = straw2)
  })
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    datachoose()
  }))
  
 
  plotinput <- reactive({
    switch(input$Var,
           "total_value" = barplot(s2$total, names.arg = s2$State),
           "total_year_state" = ggplot(data= s3,aes(x=Year,y=total,group=State,color=State))+ 
             geom_line()+ 
             geom_point() 
           
    ) })
  
  output$selectVar <- renderPlot({
    plotinput()
  })
  
 
}



shinyApp(ui = ui, server = server)
