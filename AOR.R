#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)
library(shinycssloaders)

sqlcode='
select 
firstname
,lastname
,employeeid
,reportingnamelevel1
,reportinglevel1
,reportingnamelevel2
,reportinglevel2
,reportingnamelevel3
,reportinglevel3
,reportingnamelevel4
,reportinglevel4
,reportingnamelevel5
,reportinglevel5
,reportingnamelevel6
,reportinglevel6
,reportingnamelevel7
,reportinglevel7
,cOUNTRYNAME
from "HCM"."ANALYTICS"."VW_HCM_MONTHLYKPI"
where snapshotmonth=(select max(snapshotmonth) from "VW_HCM_MONTHLYKPI")'

myconn_wr <- DBI::dbConnect(odbc::odbc(),
                            Driver       = "SnowflakeDSIIDriver",
                            Server       = Sys.getenv("server"),
                            UID          = Sys.getenv("uid"),
                            PWD          = Sys.getenv("wr"),     #Sys.getenv("wr"), # find from Renviron variable
                            Database     = Sys.getenv("db"),
                            Warehouse    = Sys.getenv("wh"))
                            Schema       = Sys.getenv("schema"))
                            Role         = Sys.getenv("role"))

myconn <- DBI::dbConnect(odbc::odbc(),
                         Driver       = "SnowflakeDSIIDriver",
                         Server       = Sys.getenv("server"),
                         UID          = Sys.getenv("uid"),
                         PWD          = #Sys.getenv("rd"),    #Sys.getenv("rd"), # find from Renviron variable
                         Database     = Sys.getenv("db"),
                         Warehouse    = Sys.getenv("wh"))
                         Schema       = Sys.getenv("schema"))
                         Role         = Sys.getenv("role2"))
)

mydata <- DBI::dbGetQuery(myconn,sqlcode)

mydata$RE1<-str_c(mydata$REPORTINGNAMELEVEL1," ",mydata$REPORTINGLEVEL1)
mydata$RE2<-str_c(mydata$REPORTINGNAMELEVEL2,", ",mydata$REPORTINGLEVEL2)
mydata$RE3<-str_c(mydata$REPORTINGNAMELEVEL3,", ",mydata$REPORTINGLEVEL3)
lvl1leader<-unique(mydata$RE1)
lvl2leader<-unique(mydata$RE2)
lvl3leader<-unique(mydata$RE3)
#
country<-unique(mydata['COUNTRYNAME']

hrbpname<-str_c(hrbpdf$LASTNAME,", ",hrbpdf$FIRSTNAME," ", hrbpdf$EMPLOYEEID)

desired_length <- 0 # or whatever length you want
appleaderlist <- rep(NA, desired_length)
secleaderlist <- rep(NA, desired_length)
hrbplist <- rep(NA, desired_length)
countrylist <- rep(NA, desired_length)

for (i in lvl1leader)
{appleaderlist<-c(appleaderlist,i)
secleaderlist<-c(secleaderlist,i)
}
for (i in lvl2leader)
{appleaderlist<-c(appleaderlist,i)}
for (i in lvl3leader)
{appleaderlist<-c(appleaderlist,i)}

for (i in country)
{countrylist<-c(countrylist,i)}

for (i in hrbpname)
{hrbplist<-c(hrbplist,i)}

secleaderlist<-secleaderlist[order(secleaderlist)]

appleaderlist<-appleaderlist[order(appleaderlist)]
countrylist<-countrylist[order(countrylist)]
hrbplist<-hrbplist[order(hrbplist)]

readsecsql="select * from hrbpmatrix_secaor "
writesec <- Id(database', schema, table='HRBPMATRIX_SECAOR') 
writedel <- Id(database, schema', table='HRBPMATRIX_DEL')

this_table <- DBI::dbGetQuery(myconn,readsecsql)
#df for checking duplicates
df_dup=aggregate(list(Number_of_Duplicates=rep(1,nrow(this_table ))), this_table , length)
df_dup2=df_dup[df_dup$Number_of_Duplicates>1,][,c('HRBP','LEADER','COUNTRY','Number_of_Duplicates')]

editor=c('otter@corp.com')

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
ui <- fluidPage(
  

  navbarPage(strong('HRBP Support Matrix'),
          
             tabPanel("Security Access", 
                      
                      sidebarLayout(   
                        sidebarPanel(
                          # column(12,actionButton('save','Save Changes')),
                          fluidRow(
                            column(12, align="right", #offset = 3,
                                   actionButton('save',label = 'Save',class = "btn-primary",width='150px')
                                   
                            ),
                            column(12, align="right", htmlOutput("savetext"))
                            
                            
                          ), 
                      
                          
                          h4(strong("Need to add AOR?")),
                          # textOutput("failsave"),
                          
                          selectInput("userid2","select HRBP",choices = hrbplist),
                          selectInput("leaderid2","select leader",choices = secleaderlist),
                          selectInput("countryid2","select country",choices = countrylist),
                          selectInput("startdateselection","Select Start Date",choices = seq(Sys.Date()-30, Sys.Date()+30, by="days")),
                          actionButton("add_btn", "Add New AOR",class = "btn-primary",width='150px'),
                          htmlOutput("addtext"),
                  
                          br(),
                          br(),
                          
                          br(),
                          br(),
                          h4(strong("Need to end AOR? Highlight the rows")),
                          selectInput("enddateselection","Select End Date",choices = seq(Sys.Date()-30, Sys.Date()+30, by="days")),
                          actionButton("update_enddate", "Update End Date",class = "btn-primary",width='150px'),
                          htmlOutput("endtext"),
                          br(),
                          
                          actionButton("delete_btn", "Delete AOR Error", class = "btn-danger",width='150px'),
                          
                        ),

                        mainPanel(
                          fluidRow(
                            column(12, align="right", #offset = 3,
                                   downloadButton("downloadData1", "Download File",class = "btn-success"))
                            
                          )
                          ,br()
                          ,
                          withSpinner(DTOutput("shiny_table"),type=4)
  
                        )
                      )    
             ),  # end of security tab
             tabPanel("Approval Access", 
                      sidebarPanel(selectInput("userid4","select HRBP",choices = hrbplist),
                                   selectInput("leaderid4","select leader",choices = appleaderlist),
                                   selectInput("countryid4","select country",choices = countrylist),
                                   actionButton("add_btn4", "Add New Entry"),
                                   #actionButton("delete_btn4", "Delete Existing Entries"),
                                   actionButton("void_btn4", "Void Existing Entries"),
                                   actionButton('save4','Save Changes')),
         
                      mainPanel('Please verify the matrix list when you edit. 
                                         Duplicated records will cause consequential issues in security settings.\
                                         Type in key words in the search bar.
                                         You can use combination words. e.g. "Lasondra United States". When you finish editing, make sure clicking save changes to update the records'
                                , DTOutput("shiny_table2")
                                ,actionButton("delete_btn4", "Delete Existing Entries")
                                ,downloadButton("downloadData2", "Download File")
                      )
                   
             ),  # end of approval tab
             tabPanel("How to",
                      mainPanel(
                        h4("Overall Process"),
                        br(),
                        "HRBPs to review/update both the Security Access tab and the Approval Access tab",
                        br(),
                        h4("Reference Guide"),
                        br(),
                        
                        
                        # "Go in the Security Access Tab. This is where you would need to define access to view data for your Leaders (see definition of view data below).",
                        br(),
                        img(src = "matrixrg.PNG"),
                        img(src = "matrixrg2.PNG")
                        
                      )
             )
             
  )
)

server <- function(input, output, session) {

  this_table <- reactiveVal(this_table)
  sec_dup_check <- reactiveVal(df_dup2)
  that_table <- reactiveVal(that_table)
  
  observeEvent(
    #only copy back the status for any changes happen on the logdate
    
    input$save_confirmed,{
      
      removeModal()
      if (!is.null(session$user)) {
        
        # session contorl start 
        if (
          session$user %in% editor
        ){
          
          showNotification("Saving ...",  type='message',duration=5)
          df_read=this_table()  # data table record, not the database record
          
          dbdata <- DBI::dbGetQuery(myconn,readsecsql)
          secupdate=df_read[df_read$UPDATEDATE == as.character(Sys.Date()), ]
          print("secupdate")
          print(secupdate)
          if (nrow(secupdate)>0) {
            
            newentrycheck <- secupdate$RECORD_ID %in% dbdata$RECORD_ID 
            print(newentrycheck)
            ind=which(newentrycheck == 'FALSE') # get all new AOR
            print("test ind, more than one number list means new aor")
            print(ind)
            ind2=which(newentrycheck == 'TRUE')  # get all old AOR
            if (length(ind)>0){
              for(i in ind){
                
                print(i)
                recordid=str_c("'",secupdate$RECORD_ID[i],"'")
                hrbp=str_c("'",secupdate$HRBP[i],"'")
                leader=str_c("'",secupdate$LEADER[i],"'")
                country=str_c("'",secupdate$COUNTRY[i],"'")
                status=str_c("'",secupdate$STATUS[i],"'")
                updateby=str_c("'",secupdate$UPDATEBY[i],"'")
                updatedate=str_c("'",secupdate$UPDATEDATE[i],"'")
                startdate=str_c("'",secupdate$STARTDATE[i],"'")
                if (is.na(secupdate$ENDDATE[i])){ enddate=str_c("''")} else { enddate=str_c("'",secupdate$ENDDATE[i],"'") }
                value=paste(recordid,hrbp,leader,country,status,updateby,updatedate,startdate,enddate, sep=",")
                value2=str_c("(",value,")")
                
                sqli=str_c("INSERT INTO hcm.lab.hrbpmatrix_secaor VALUES ",value2)
                print(sqli)
                dbGetQuery(myconn_wr, sqli)
                
              } # end of for loop
               showNotification("AOR saved successfully",  type='message',duration=5)
              
            }
            if (length(ind2)>0){
              oldentry=secupdate[ind2,] 
              for(i in oldentry$RECORD_ID){
                print(i)
                ri=gsub(" ", "",paste("'",i,"'"))
                status=oldentry[which(oldentry$RECORD_ID==i),5] #status
                status=gsub(" ", "",paste("'",status,"'"))
                enddate=oldentry[which(oldentry$RECORD_ID==i),9]   #end date
                enddate=gsub(" ", "",paste("'",enddate,"'"))
                logname=oldentry[which(oldentry$RECORD_ID==i),6]   #log name
                logname=gsub(" ", "",paste("'",logname,"'"))
                logdate=oldentry[which(oldentry$RECORD_ID==i),7]   #log date
                logdate=gsub(" ", "",paste("'",logdate,"'"))
                
                ws1=paste("UPDATE hrbpmatrix_secaor SET STATUS=",status," WHERE RECORD_ID=",ri)
                ws2=paste("UPDATE hrbpmatrix_secaor SET ENDDATE=",enddate," WHERE RECORD_ID=",ri)
                ws3=paste("UPDATE hhrbpmatrix_secaor SET UPDATEBY=",logname," WHERE RECORD_ID=",ri)
                ws4=paste("UPDATE hrbpmatrix_secaor SET UPDATEDATE=",logdate," WHERE RECORD_ID=",ri)
                sql1=gsub("= ","=", ws1)
                sql2=gsub("= ","=", ws2)
                sql3=gsub("= ","=", ws3)
                sql4=gsub("= ","=", ws4)
                print(sql1)
                print(sql2)
                print(sql3)
                print(sql4)
                dbGetQuery(myconn_wr, sql1)
                dbGetQuery(myconn_wr, sql2)
                dbGetQuery(myconn_wr, sql3)
                dbGetQuery(myconn_wr, sql4)
              }
              # output$savetext <- renderText({ return(paste("<span style=\"color:green\">Data saved</span>")) }) 
              showNotification("AOR saved successfully",  type='message',duration=5)
              
            }
          } else { 
            showNotification("No change is detected. Nothing saved",  type='message',duration=5)
          }  
          #end of if statement for null logdate updates
        } else { showNotification("You do not have the permission to edit",  type='warning',duration=5)}# end of if statement for session control
        
      }  # end of if statement for applying online session. 
      ##################################   
      
      else {    # statement for local app to run without session data.
        
        showNotification("Saving ...",  type='message',duration=5)
        df_read=this_table()  # data table record, not the database record
        
        dbdata <- DBI::dbGetQuery(myconn,readsecsql)
        secupdate=df_read[df_read$UPDATEDATE == as.character(Sys.Date()), ]
        print("secupdate")
        print(secupdate)
        if (nrow(secupdate)>0) {
          
          newentrycheck <- secupdate$RECORD_ID %in% dbdata$RECORD_ID 
          print(newentrycheck)
          ind=which(newentrycheck == 'FALSE') # get all new AOR
          print("test ind, more than one number list means new aor")
          print(ind)
          ind2=which(newentrycheck == 'TRUE')  # get all old AOR
          if (length(ind)>0){
            for(i in ind){
              
              print(i)
              recordid=str_c("'",secupdate$RECORD_ID[i],"'")
              hrbp=str_c("'",secupdate$HRBP[i],"'")
              leader=str_c("'",secupdate$LEADER[i],"'")
              country=str_c("'",secupdate$COUNTRY[i],"'")
              status=str_c("'",secupdate$STATUS[i],"'")
              updateby=str_c("'",secupdate$UPDATEBY[i],"'")
              updatedate=str_c("'",secupdate$UPDATEDATE[i],"'")
              startdate=str_c("'",secupdate$STARTDATE[i],"'")
              if (is.na(secupdate$ENDDATE[i])){ enddate=str_c("''")} else { enddate=str_c("'",secupdate$ENDDATE[i],"'") }
             value=paste(recordid,hrbp,leader,country,status,updateby,updatedate,startdate,enddate, sep=",")
        
              value2=str_c("(",value,")")
              
              sqli=str_c("INSERT INTO hcm.lab.hrbpmatrix_secaor VALUES ",value2)
              print(sqli)
              dbGetQuery(myconn_wr, sqli)
              
            } # end of for loop
            showNotification("AOR saved successfully",  type='message',duration=5)
            
          }
          if (length(ind2)>0){
            oldentry=secupdate[ind2,] 
            for(i in oldentry$RECORD_ID){
              print(i)
              ri=gsub(" ", "",paste("'",i,"'"))
              status=oldentry[which(oldentry$RECORD_ID==i),5] #status
              status=gsub(" ", "",paste("'",status,"'"))
              enddate=oldentry[which(oldentry$RECORD_ID==i),9]   #end date
              enddate=gsub(" ", "",paste("'",enddate,"'"))
              logname=oldentry[which(oldentry$RECORD_ID==i),6]   #log name
              logname=gsub(" ", "",paste("'",logname,"'"))
              logdate=oldentry[which(oldentry$RECORD_ID==i),7]   #log date
              logdate=gsub(" ", "",paste("'",logdate,"'"))
               ws1=paste("UPDATE hcm.lab.hrbpmatrix_secaor SET STATUS=",status," WHERE RECORD_ID=",ri)
              ws2=paste("UPDATE hcm.lab.hrbpmatrix_secaor SET ENDDATE=",enddate," WHERE RECORD_ID=",ri)
              ws3=paste("UPDATE hcm.lab.hrbpmatrix_secaor SET UPDATEBY=",logname," WHERE RECORD_ID=",ri)
              ws4=paste("UPDATE hcm.lab.hrbpmatrix_secaor SET UPDATEDATE=",logdate," WHERE RECORD_ID=",ri)
              sql1=gsub("= ","=", ws1)
              sql2=gsub("= ","=", ws2)
              sql3=gsub("= ","=", ws3)
              sql4=gsub("= ","=", ws4)
              print(sql1)
              print(sql2)
              print(sql3)
              print(sql4)
              dbGetQuery(myconn_wr, sql1)
              dbGetQuery(myconn_wr, sql2)
              dbGetQuery(myconn_wr, sql3)
              dbGetQuery(myconn_wr, sql4)
            }
            # output$savetext <- renderText({ return(paste("<span style=\"color:green\">Data saved</span>")) }) 
            showNotification("AOR saved successfully",  type='message',duration=5)
            
          }
        } else { 
          # output$savetext <- renderText({ return(paste("<span style=\"color:navy\">No update yet.Nothing saved</span>")) }) 
          showNotification("No change is detected. Nothing saved",  type='message',duration=5)
        }  
        #end of if statement for null logdate updates  
      } # end of else statement for local session 
      
      #################################   
    } #end save action
  ) #EDW sec
  
  observeEvent(input$save4,{write.csv(that_table(),'C:\\Users\\YGong\\Documents\\R codes\\matrix_hrbp.csv', row.names = FALSE)}) #local drive
  
  
  
  
  # add button for sec AOR
  observeEvent(input$add_btn, {
    #verify if it's published version. only published version can track user session. is.null for session means it's local version
    if (!is.null(session$user)) {
      
      # session contorl start 
    
      if (
        session$user %in% editor
      ){
        
        if (is.null(session$user)) { 
          user="nulluser"
        } else {user=session$user}
        recordid=gsub(" ","",paste(str_sub(input$userid2,-6),'_', str_sub(input$leaderid2,-6),'_',input$countryid2))
        
        df3<-this_table()
        df3<- reactiveVal(df3)
        print('checked this step1')
        
        
        t = rbind(data.frame(
          RECORD_ID= recordid,
          HRBP= input$userid2,
          LEADER = input$leaderid2,
          COUNTRY = input$countryid2,
          STATUS="Active",
          UPDATEBY = user,
          UPDATEDATE = Sys.Date(),
          STARTDATE = input$startdateselection,
          ENDDATE=""
        ), df3())
        
        
        df3(t)
        # #read the table after adding line, check duplication of leader+country
        
        print('checked this step2')
        act=df3()
        ind=which(act$STATUS== 'Active') # get all active AOR
        
        # if (length(ind)>0){
        
        df2=act[ind,] 
        
        # df2 = df3()
        hrbpleadercountry<-str_c(df2$HRBP,"-",df2$LEADER,"-",df2$COUNTRY)
        if(anyDuplicated(hrbpleadercountry)!=0 ){
    
          p1<-df2[anyDuplicated(hrbpleadercountry),2]
          p2<-df2[anyDuplicated(hrbpleadercountry),3]
          p3<-df2[anyDuplicated(hrbpleadercountry),4]
          duphrbp1<-str_c(p1,", ",p2,", ",p3)
          # showNotification("At least one duplication entry is found. One set is", duphrbp1, type='error',duration=NULL)
          showNotification("The new entry is duplicating with an existing entry. Please check carefully", duphrbp1, type='error',duration=NULL)
          output$addtext <- renderText({ return(paste("<span style=\"color:navy\">New AOR failed</span>")) })
        } 
        else {
          # showNotification("No duplication has been detected",  type='message',duration=5)
          showNotification("New Entry applied",  type='message',duration=5)
          t = rbind(data.frame(
            RECORD_ID= recordid,
            HRBP= input$userid2,
            LEADER = input$leaderid2,
            COUNTRY = input$countryid2,
            STATUS="Active",
            UPDATEBY = user,
            UPDATEDATE = Sys.Date(),
            STARTDATE = input$startdateselection,
            ENDDATE=""), this_table())
          this_table(t)
          
          showNotification("New AOR created",  type='message',duration=5)
        }
        # apply session control end
      } else {  showNotification("You do not have the permission to edit",  type='warning',duration=5)  }
      
    } else {    # statement for local app to run without session data.
      
      if (is.null(session$user)) { 
        user="nulluser"
      } else {user=session$user}
      recordid=gsub(" ","",paste(str_sub(input$userid2,-6),'_', str_sub(input$leaderid2,-6),'_',input$countryid2))
      
      df3<-this_table()
      df3<- reactiveVal(df3)
      print('checked this step1')
      
      
      t = rbind(data.frame(
        RECORD_ID= recordid,
        HRBP= input$userid2,
        LEADER = input$leaderid2,
        COUNTRY = input$countryid2,
        STATUS="Active",
        UPDATEBY = user,
        UPDATEDATE = Sys.Date(),
        STARTDATE = input$startdateselection,
        ENDDATE=""
      ), df3())
      
      
      df3(t)
      # #read the table after adding line, check duplication of leader+country
      
      print('checked this step2')
      act=df3()
      ind=which(act$STATUS== 'Active') # get all active AOR
      
      # if (length(ind)>0){
      
      df2=act[ind,] 
      
      # df2 = df3()
      hrbpleadercountry<-str_c(df2$HRBP,"-",df2$LEADER,"-",df2$COUNTRY)
      if(anyDuplicated(hrbpleadercountry)!=0 ){
        
        
        p1<-df2[anyDuplicated(hrbpleadercountry),2]
        p2<-df2[anyDuplicated(hrbpleadercountry),3]
        p3<-df2[anyDuplicated(hrbpleadercountry),4]
        duphrbp1<-str_c(p1,", ",p2,", ",p3)
        # showNotification("At least one duplication entry is found. One set is", duphrbp1, type='error',duration=NULL)
        showNotification("The new entry is duplicating with an existing entry. Please check carefully", duphrbp1, type='error',duration=NULL)
        output$addtext <- renderText({ return(paste("<span style=\"color:navy\">New AOR failed</span>")) })
      } 
      else {
        # showNotification("No duplication has been detected",  type='message',duration=5)
        showNotification("New Entry applied",  type='message',duration=5)
        t = rbind(data.frame(
          RECORD_ID= recordid,
          HRBP= input$userid2,
          LEADER = input$leaderid2,
          COUNTRY = input$countryid2,
          STATUS="Active",
          UPDATEBY = user,
          UPDATEDATE = Sys.Date(),
          STARTDATE = input$startdateselection,
          ENDDATE=""), this_table())
        this_table(t)
        
        showNotification("New AOR created",  type='message',duration=5)
      }     # end of else for local version

    }# end of if else statement for checking local/online version.
  })

  #mimic same server side setting for HRBP tab
  observeEvent(input$add_btn4, {
    t2 = rbind(data.frame(HRBP= input$userid4,
                          Leader = input$leaderid4,
                          Country = input$countryid4,
                          Status="Active",
                          UPDATEBY = "Test user",
                          UPDATEDATE = Sys.Date()), that_table())
    that_table(t2)
    
    # #read the table after adding line, check duplication of leader+country
    df = that_table()
    leadercountry<-str_c(df$Leader,"-",df$Country)
    if(anyDuplicated(leadercountry)!=0 ){
      
      dupleader1<-df[anyDuplicated(leadercountry),2]
      showNotification("At least one duplication is found. One Leader is ",dupleader1, type='error',duration=NULL)
      
    }else if(session$user!='YGong@corp.intusurg.com'){
      showNotification("No duplication has been detected",  type='message',duration=5)
    }
  })
  
  
  #delete row button
 
  observeEvent(input$delete_confirmed, {
 t = this_table()
    actsql="select * from HCM.LAB.hrbpmatrix_secaor"
    actdata <- DBI::dbGetQuery(myconn,actsql) 
    selectedid=t$RECORD_ID[input$shiny_table_rows_selected]
    del_id=selectedid %in% actdata$RECORD_ID   # search for ID that pre-exist in table and delete them.
    ind=which(del_id=="TRUE")
    if (length(ind)>0) {
      id=selectedid[ind]
      id2=paste("'",id,"'",collapse=", ",sep="")
      
      delsql=paste( "DELETE FROM HCM.LAB.hrbpmatrix_secaor WHERE RECORD_ID in (",id2, ")")
      print(delsql)
      dbGetQuery(myconn_wr, delsql)   #delete from hrbpmatrix table
    }  #end of if statement, if find at least 1 ID from table then delete it.
    if  ( length(ind)>0 ) {  }
    print(nrow(t))
    if (!is.null(input$shiny_table_rows_selected)) {
      print(t[as.numeric(input$shiny_table_rows_selected),])
      if (is.null(session$user)) { 
        user="nulluser"
      } else {user=session$user}
      
      t$UPDATEBY[input$shiny_table_rows_selected]=user
      t$UPDATEDATE[input$shiny_table_rows_selected]=as.character(Sys.Date())
   selected=input$shiny_table_rows_selected
      if (length(selected)>0){
        for(i in selected){
          
          print(i)
          recordid=str_c("'",t$RECORD_ID[i],"'")
          hrbp=str_c("'",t$HRBP[i],"'")
          leader=str_c("'",t$LEADER[i],"'")
          country=str_c("'",t$COUNTRY[i],"'")
          status=str_c("'",t$STATUS[i],"'")
          updateby=str_c("'",t$UPDATEBY[i],"'")
          updatedate=str_c("'",t$UPDATEDATE[i],"'")
          startdate=str_c("'",t$STARTDATE[i],"'")
          if (is.na(t$ENDDATE[i])){ enddate=str_c("''")} else { enddate=str_c("'",t$ENDDATE[i],"'") }
          value=paste(recordid,hrbp,leader,country,status,updateby,updatedate,startdate,enddate, sep=",")
          
          value2=str_c("(",value,")")
          
          sqli=str_c("INSERT INTO hcm.lab.hrbpmatrix_del VALUES ",value2)
          print(sqli)
          dbGetQuery(myconn_wr, sqli)
          
        } # end of for loop
        # output$savetext <- renderText({ return(paste("<span style=\"color:navy\">Data deleted</span>")) }) 
        showNotification("AOR deleted",  type='message',duration=5)
      }
    
      t <- t[-as.numeric(input$shiny_table_rows_selected),]
    }
    this_table(t)
    removeModal()
    showNotification("Selected Entry Deleted",  type='message',duration=5)
   
  })
  
  #mimic same server side setting for HRBP tab
  observeEvent(input$delete_btn4, {
    t2 = that_table()
    print(nrow(t2))
    if (!is.null(input$shiny_table2_rows_selected)) {
      t2 <- t2[-as.numeric(input$shiny_table2_rows_selected),]
    }
    that_table(t2)
    
    # #read the table after adding line, check duplication of leader+country
    df = that_table()
    leadercountry<-str_c(df$Leader,"-",df$Country)
    if(anyDuplicated(leadercountry)!=0 ){
      
      dupleader1<-df[anyDuplicated(leadercountry),2]
      showNotification("At least one duplication is found. One Leader is ",dupleader1, type='error',duration=NULL)
      
    }else {
      showNotification("No duplication has been detected",  type='message',duration=5)
    }
  })
  
  
  output$shiny_table <- renderDT({
    print(input$save_confirmed)
    datatable(this_table(), 
              selection = 'multiple',
              rownames = FALSE,
              filter = list(
                position = 'top', clear = FALSE
              ),
              options =
                list(
                  pageLength = 20,
                  searchCols = list( NULL, NULL, NULL,NULL,
                                     list(search = "Active"), NULL, NULL)
                )
              
    )
    
  })
  
  #mimic for HRBP tab
  output$shiny_table2 <- renderDT({
    datatable(that_table(), 
              selection = 'multiple',
              filter = list(
                position = 'top', clear = FALSE
              ),
              options = 
                list(
                  pageLength = 20,
                  searchCols = list(NULL, NULL, NULL, NULL,
                                    list(search = "Active"), NULL, NULL)
                )
              
    )
    
  })

  observeEvent(
    
    input$on,{
      output$sec_dup <- renderDT({
        datatable(
          sec_dup_check()
        )
      })
      
      
    }) #EDW sec
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('Security Access', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(this_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('Approval Access', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(that_table(), file, row.names = FALSE)
    }
  )
 
 
  #modifying status, logdate for AA
  observeEvent(input$void_btn4, {
    t4 = that_table()
    t4$Status[input$shiny_table2_rows_selected]="Void"
    t4$UPDATEBY[input$shiny_table2_rows_selected]="Tester"
    t4$UPDATEDATE[input$shiny_table2_rows_selected]=Sys.Date()
    
    that_table(t4)
 
  })

  observeEvent(input$update_enddate, {
    
    #verify if it's published version. only published version can track user session. is.null for session means it's local version
    if (!is.null(session$user)) {
      
      # session control start 
      if (
        session$user %in% editor
      ){
        t3 = this_table()
        print(is.na(t3$ENDDATE[input$shiny_table_rows_selected]))  #length measure how many rows , is.na measure if na.  is.na not = is.null
        
        for (i in input$shiny_table_rows_selected){
          
          if (
            is.na(t3$ENDDATE[i]) |  t3$ENDDATE[i]==""
            
          ){
            
            
            t3$STATUS[i]="Ended"
            if (is.null(session$user)) { 
              user="nulluser"
            } else {user=session$user}
            t3$UPDATEBY[i]=user
            t3$UPDATEDATE[i]=as.character(Sys.Date())
            t3$ENDDATE[i]=as.character(input$enddateselection)
            
            output$endtext <- renderText({ return(paste("<span style=\"color:navy\">AOR ended</span>"))})
            
          } else { output$endtext <- renderText({ return(paste("<span style=\"color:orange\">AOR was already ended. Please verify</span>"))}) }
          
        } #end of for loop
        this_table(t3)
      
      } else {      showNotification("You do not have the permission to edit",  type='warning',duration=5)               } #session control ends
   
    } # end of if online
    else {    # statement for local app to run without session data.
      
      t3 = this_table()
      print(is.na(t3$ENDDATE[input$shiny_table_rows_selected]))  #length measure how many rows , is.na measure if na.  is.na not = is.null
      
      for (i in input$shiny_table_rows_selected){
        
        if (
          is.na(t3$ENDDATE[i]) |  t3$ENDDATE[i]==""
          
        ){
          
          
          t3$STATUS[i]="Ended"
          if (is.null(session$user)) { 
            user="nulluser"
          } else {user=session$user}
          t3$UPDATEBY[i]=user
          t3$UPDATEDATE[i]=as.character(Sys.Date())
          t3$ENDDATE[i]=as.character(input$enddateselection)
          
          output$endtext <- renderText({ return(paste("<span style=\"color:navy\">AOR ended</span>"))})
          
        } else { output$endtext <- renderText({ return(paste("<span style=\"color:orange\">AOR was already ended. Please verify</span>"))}) }
        
      } #end of for loop
      this_table(t3)
      
    } # end of else statement for local
    
    
    
  })
 
  observeEvent(input$delete_btn, {
    #verify if it's published version. only published version can track user session. is.null for session means it's local version
    if (!is.null(session$user)) {
      
      # session contorl start 
      if (
        session$user %in% editor
      ){
        
        t=this_table()
        actsql="select * from HCM.LAB.hrbpmatrix_secaor where status='Active' "
        actdata <- DBI::dbGetQuery(myconn,actsql) 
        if (!is.null(input$shiny_table_rows_selected)) {    #check if deleted rows is from db
          
          del_id=t$RECORD_ID[input$shiny_table_rows_selected] %in% actdata$RECORD_ID
          ind=which(del_id=="TRUE")
          
          if  ( length(ind)>0 )    # if highlight ID is active in HCM, pop msg to warn.
          {msg="The AOR you selected is active in HCM. Do you want to try ending it instead?"   }else { msg="You are about to remove the record."  }
          
          showModal(modalDialog(
            title="Do you confirm the deletion?",
            msg,
            footer = tagList(actionButton("delete_confirmed", "Confirm"),
                             modalButton("Cancel")
            )
          )) } else { showNotification("You have to highlight the deletion first", type='error',duration=NULL) } #end checking if highlight any rows.
      } else {  showNotification("You do not have the permission to edit",  type='warning',duration=5)  }
      
    } else {    # statement for local app to run without session data.
      
      t=this_table()
      actsql="select * from HCM.LAB.hrbpmatrix_secaor where status='Active' "
      actdata <- DBI::dbGetQuery(myconn,actsql) 
      if (!is.null(input$shiny_table_rows_selected)) {    #check if deleted rows is from db
        
        del_id=t$RECORD_ID[input$shiny_table_rows_selected] %in% actdata$RECORD_ID
        ind=which(del_id=="TRUE")
        
        if  ( length(ind)>0 )    # if highlight ID is active in HCM, pop msg to warn.
        {msg="The AOR you selected is active in HCM. Do you want to try ending it instead?"   }else { msg="You are about to remove the record."  }
        
        showModal(modalDialog(
         title="Do you confirm the deletion?",
          msg,
          footer = tagList(actionButton("delete_confirmed", "Confirm"),
                           modalButton("Cancel")
          )
        )) } else { showNotification("You have to highlight the deletion first", type='error',duration=NULL) } #end checking if highlight any rows.
      
      
    }
  })
  
  observeEvent(input$save, {
    #verify if it's published version. only published version can track user session. is.null for session means it's local version
    if (!is.null(session$user)) {
      
      # session contorl start 
      if (
        session$user %in% editor
      ){
        showModal(modalDialog(
      
          title="Save changes",
          "Please make sure the modification is correct.",
          footer = tagList(actionButton("save_confirmed", "Save"),
                           modalButton("Cancel")
          )
        )) } else { showNotification("You do not have the permission to edit",  type='warning',duration=5)  }
 } else {
   showModal(modalDialog(
     
     title="Save changes",
     "Please make sure the modification is correct.",
     footer = tagList(actionButton("save_confirmed", "Save"),
                      modalButton("Cancel")
     )
   )) 
   
 }# end of session if statement
  } )# end of save action
  
  

} #end of server scripts
shinyApp(ui = ui, server = server)

