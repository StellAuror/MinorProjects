library(bs4Dash)
library(shiny)
library(htmlwidgets)
library(htmltools)
library(rvest)
library(tidyverse)
library(tuber)
library(stringr)
library(SnowballC)
library(tm)
library(qdap)
library(wordcloud2)

client_id <- ""


client_secret <- "GOCSPX-"

# use the youtube oauth 
yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

### App ###
shinyApp(
  ui <- bs4DashPage(
    head = bs4DashNavbar(
      
    ),
    body = bs4DashBody(
      #includeCSS("style.css"),
      tabItems(
        tabItem(
          tabName = "comments",
          fluidPage(
            fluidRow(
              column(
                width = 8,
                box(
                  width = 12,
                  solidHeader = T,
                  status = "gray-dark",
                  maximizable = T,
                  collapsible = T,
                  DT::dataTableOutput("channel.ids")
                ),
                box(
                  width = 12,
                  solidHeader = T,
                  status = "gray-dark",
                  maximizable = T,
                  collapsible = T,
                  collapsed = T,
                  wordcloud2::wordcloud2Output("comments.wcloud")
                )
              ),
              column(
                width = 4,
                box(
                  width = 12,
                  solidHeader = T,
                  status = "gray-dark",
                  maximizable = T,
                  collapsible = T,
                  textInput(
                    inputId = "cha.id",
                    label = "Enter YouTube channel id",
                    width = "100%",
                    value = "UC7RswyY8VfbSdikz_8wdp3w"
                  ),
                  textInput(
                    inputId = "vid.id",
                    label = "Enter YouTube video id",
                    width = "100%",
                    value = ""
                  )
                )
              )
            )
          )
        )
      )
    ),
    sidebar = bs4DashSidebar(
      sidebarMenu(
        id = "sbar",
        menuItem(
          tabName = "comments",
          icon = icon("user"),
          text = "Comments"
        )
      )
    ),
    control = bs4DashControlbar(
      
    )
  ),
  server = function(output, input, session) {
  ### Comments wordcloud visualization
    output$comments.wcloud <- wordcloud2::renderWordcloud2({
      stopwords_pl <- 
        c("a","aby","ach","acz","aczkolwiek","aj","albo","ale","ależ","ani","aż","bardziej","bardzo","bez","bo",
          "bowiem","by","byli","bym","bynajmniej","być","był","była","było","były","będzie","będą","cali","cała",
          "cały","chce","choć","ci","ciebie","cię","co","cokolwiek","coraz","coś","czasami","czasem","czemu","czy",
          "czyli","często","daleko","dla","dlaczego","dlatego","do","dobrze","dokąd","dość","dr","dużo","dwa","dwaj",
          "dwie","dwoje","dzisiaj","dziś","gdy","gdyby","gdyż","gdzie","gdziekolwiek","gdzieś","go","godz","hab","i",
          "ich","ii","iii","ile","im","inna","inne","inny","innych","inż","iv","ix","iż","ja","jak","jakaś","jakby",
          "jaki","jakichś","jakie","jakiś","jakiż","jakkolwiek","jako","jakoś","je","jeden","jedna","jednak",
          "jednakże","jedno","jednym","jedynie","jego","jej","jemu","jest","jestem","jeszcze","jeśli","jeżeli",
          "już","ją","każdy","kiedy","kierunku","kilka","kilku","kimś","kto","ktokolwiek","ktoś","która","które",
          "którego","której","który","których","którym","którzy","ku","lat","lecz","lub","ma","mają","mam","mamy",
          "mało","mgr","mi","miał","mimo","między","mnie","mną","mogą","moi","moim","moja","moje","może","możliwe",
          "można","mu","musi","my","mój","na","nad","nam","nami","nas","nasi","nasz","nasza","nasze","naszego",
          "naszych","natomiast","natychmiast","nawet","nic","nich","nie","niech","niego","niej","niemu","nigdy","nim",
          "nimi","nią","niż","no","nowe","np","nr","o","o.o.","obok","od","ok","około","on","ona","one","oni","ono",
          "oraz","oto","owszem","pan","pana","pani","pl","po","pod","podczas","pomimo","ponad","ponieważ","powinien",
          "powinna","powinni","powinno","poza","prawie","prof","przecież","przed","przede","przedtem","przez","przy",
          "raz","razie","roku","również","sam","sama","się","skąd","sobie","sobą","sposób","swoje","są","ta","tak",
          "taka","taki","takich","takie","także","tam","te","tego","tej","tel","temu","ten","teraz","też","to","tobie",
          "tobą","toteż","totobą","trzeba","tu","tutaj","twoi","twoim","twoja","twoje","twym","twój","ty","tych","tylko",
          "tym","tys","tzw","tę","u","ul","vi","vii","viii","vol","w","wam","wami","was","wasi","wasz","wasza","wasze",
          "we","według","wie","wiele","wielu","więc","więcej","wszyscy","wszystkich","wszystkie","wszystkim","wszystko",
          "wtedy","www","wy","właśnie","wśród","xi","xii","xiii","xiv","xv","z","za","zapewne","zawsze","zaś","ze",
          "zeznowu","znowu","znów","został","zł","żaden","żadna","żadne","żadnych","że","żeby")
      
      Comments <- get_all_comments(input$vid.id) #
      Comments <- paste0(Comments$textDisplay, collapse = " ")
      
      Comment.Words <- Comments %>% tolower %>% removeWords(., stopwords("en")) %>% bracketX %>% removeNumbers() %>%
        removePunctuation %>% strsplit(., split = " ")
      
      Comment.Words <- Comment.Words[[1]]
      Comment.Words <- data.frame(
        words = Comment.Words
      ) %>% group_by(words) %>% summarise(weight = sum(n())) 
      
      wordcloud2(Comment.Words, size = 15, color = "random-light", shape = "circle")
    })
    
  ### Channel vids. table
    output$channel.ids <- DT::renderDataTable({
      req(input$cha.id)
      # Channel details
      Channel <- list_channel_resources(
        filter = c(channel_id = input$cha.id),
        part = "contentDetails"
      )
      # Playlist ID
      Channel.Playlist.ID <- 
        Channel$items[[1]]$
        contentDetails$
        relatedPlaylists$
        uploads
      # Get videos on the playlist
      Channel.Videos <- 
        get_playlist_items(
          filter = c(playlist_id = Channel.Playlist.ID),
          max_results = 10
        ) 
      # Video ids
      Channel.Videos.ID <- as.vector(Channel.Videos$contentDetails.videoId)
      # Vectorizing issues
      get_all_stats <- function(id) {
        get_stats(id)
      } 
      Channel.Stats <- lapply(
        Channel.Videos.ID,
        get_stats
      )
      # As a data.frame
      Channel.Stats <- 
        do.call(
          rbind,
          lapply(Channel.Stats, data.frame)
        )
      
      # see
      Channel.Stats %>% mutate(commentCount = as.numeric(commentCount)) %>%
        arrange(-commentCount) %>%
        DT::datatable()
    })
  }
)