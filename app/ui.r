## Load Packages -----
library("shiny")
library("leaflet")
library("dplyr")
library("RColorBrewer")
library("stringr")
library("parcoords")
library("ggplot2")
library("reshape2")
library("geosphere")
library("ggthemes")
library("formattable")
library("base64enc")
library("plotly")
library('shinydashboard')

#devtools::install_github("timelyportfolio/parcoords")




##Import Data -----
#final_hos<-read.csv('E:/GitHub/shiny app/cleaned_data.csv',stringsAsFactors = F)
load("./hos.RData")
load("./importance.RData")
load("./df.RData")
load("./hospital_ratings.RData")
load("./plot1data.RData")
load("./f.RData")
##1.Find Hospital -----
tab1 <- tabPanel("Find Your Hospital",
                 
                 #CSS file for page style
                 includeCSS("theme.css"),
                 tags$div(
                 leafletOutput("intermap",width="100%", height= "600px"),
                 absolutePanel(id = "controls", class ="City_Carrier_panel panel panel-default", fixed =F,
                               draggable =FALSE, top = 80, left = "auto", right = 20,
                               bottom = "auto", width = 320, height = "auto",
                               h2("Preference Selection"),
                               selectInput("state", label = "State", 
                                           choices = c("Select","AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
                                                       "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
                                                       "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
                                                       "TX","UT","VT","VA","WA","WV","WI","WY"), selected = "Select"),
                               selectInput("type", label = "Disease Type",
                                           choices = c("Nervous system" ,"Respiratory System","Circulatory System",
                                                       "Digestive System" ,  "Hepatobiliary System & Pancreas"   ,
                                                       "Musculoskeletal System & Connective Tissue",
                                                       "Skin, Subcutaneous Tissue & Breast" ,
                                                       " Endocrine, Nutritional & Metabolic System" ,
                                                       " Kidney & Urinary Tract" ,
                                                       " Blood, Blood Forming Organs & Immunological Disorders",
                                                       " Infectious & Parasitic Disease & Disorders"  ,
                                                       " Mental Diseases & Disorders",
                                                       " Injuries, Poison & Toxic Effects of Drugs" ,
                                                       " Factors influencing Health Status",
                                                       "Ear, Nose, Mouth & Throat"), selected = "Ear, Nose, Mouth & Throat"),
                               radioButtons("care1",label = "Mortality",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care2",label = "Safety of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care3",label = "Readmission Rate",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care4",label = "Patient Experience",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care5",label = "Effectiveness of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care6",label = "Timeliness of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T),
                               radioButtons("care7",label = "Efficient Use of Medical Imaging",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 1, inline = T)),
                 tabBox(width = 12,
                        tabPanel('Personalized Ranking',
                                 dataTableOutput("tablerank"),
                                 tags$style(type="text/css", '#myTable tfoot {display:none;}')
                        ))))
          

##2.Descriptive Statistics -----
tab2 <- navbarMenu("Find More Insights",
                  
                   tabPanel(title = "Total Cost By Disease Type",
                            h3("Total Cost (USD) By MDC",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotlyOutput("map")),
                                                column(width = 3,  selectInput("sub",
                                                                               label = "Select MDC (Disease Type)",
                                                                               choices = unique(hospital$sub)),
                                                       helpText('The Major Diagnostic Categories (MDC) are formed by dividing all 
                                                                possible principal diagnoses (from ICD-9-CM) into 25 mutually exclusive 
                                                                diagnosis areas. MDC codes, like diagnosis-related group (DRG) codes, 
                                                                are primarily a claims and administrative data element unique to the 
                                                                United States medical care reimbursement system. DRG codes also are mapped, 
                                                                or grouped, into MDC codes.',
                                                                br(),
                                                                br(),
                                                                'Obviously, charges vary with different MDC because 
                                                                disparate disease need different degree of treatment.',
                                                                br(),
                                                                'Here,you can explore the discrepancy of average total cost between different ownerships.')
                                                ))
                            )),
                   
                   tabPanel(title = "Total Cost By Ownership",
                            h3("Total Cost (USD) By Ownership",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotlyOutput("map1")),
                                                column(width = 3,  selectInput("sub1",
                                                                               label = "Select a type of Ownership",
                                                                               choices = unique(hospital$Hospital.Ownership),
                                                                               selected =  unique(hospital$Hospital.Ownership)[2]),
                                                       helpText('Usually, charges vary with different sorts of ownership.',
                                                                br(),
                                                                'Here,you can explore the discrepancy of average total cost between different ownerships.')
                                                       ))
                            )),
                   
                   tabPanel(title = "Number of Hospitals of Each State",
                            h3("Number of Hospitals of Each State",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                plotlyOutput("HosNumByState")))))
                                               


##3.Insturction -----
tab3 <-navbarMenu("Insturction",
    tabPanel(title="Introduction",
                fluidRow(
                  wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                            h1("Introduction"),
                            p("Greetings! If you are thinking of finding a hospital you can go, 
                              you can just save your time and look at our app. Our group has created an 
                              app helping you to find the best hospitals around you based on your preference on 7 aspects
                              of hospitals including mortality, safety of care, readmission rate, patient experience, 
                              effectiveness of care, timeliness of care and efficient use of medical imaging. 
                              With your choice. You can see what they measures respectively below. 
                              It will be so easy to find the one fits you the best."),
                            h3("Measurements"),
                            br(),
                            strong("Mortality:"),p("the death rate of patients"),
                            br(),
                            strong("Safety of care:"),p(" the rate of certain complications and infections"),
                            br(),
                            strong("Readmission rate:"),p("the rate of unplanned readmission after treatment"),
                            br(),
                            strong("Patient experience"),p("how well patients feel during treatment, surgery and hospitalization"),
                            br(),
                            strong("Effectiveness of care"),p("how appropriately patients are treated"),
                            br(),
                            strong("Timeliness of care"),p("the time patients waiting"),
                            br(),
                            strong("Efficient use of medical imaging"),p("how efficiently the hospitals using medical imaging such as MRI and CT scans"),
                            br(),
                            p("For more information, click the link below:"),
                            a("Here",href = "https://www.medicare.gov/hospitalcompare/Data/Measure-groups.html")))),
    
    tabPanel(title="User Guide",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                            h1("User Guide"),
                            strong("Step1:"),p("Choose the state and types of dieases"),
                            br(),
                            strong("Step2:"),p("Choose your preferences of hospital"),
                            br(),
                            strong("Step3:"),p("Check the Personalized Ranking table for the basic information of all hospitals"),
                            br(),
                            strong("Step4:"),p("Click on the map to see the exact location of the hospital"),
                            br(),
                            strong("Step5:"),p("You can also locate yourself by clicking the location button on the left side of the map")))),
    tabPanel(title="Developers",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                         h1("Developers"),
                         strong("Wang, Guanren, gw2380@columbia.edu"), br(),
                         strong("Zhong, Ming, mz2692@columbia.edu"), br(),
                            strong("Cai, Zongbo, zc2455@columbia.edu"), br(),
                            strong("Li, Jingyue, jl5283@columbia.edu")
                            ))))




## UI 
ui <- shinyUI(navbarPage(title = strong("Healthcare Intelligence"),
                         tab1,
                         tab2,
                         tab3
))


