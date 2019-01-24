rm(list=ls())
library(readr)
library(dplyr)
library(readxl)
library(dygraphs)
library(xts)
#setwd("D:/Universita/Statistical Learning/Progetto")

Sondaggi <- read_excel("Sondaggi.xlsx") #data set on github
View(Sondaggi)



Sondaggi[which(Sondaggi$Data< "2018-01-01"),-c(1,2)] <- Sondaggi[which(Sondaggi$Data< "2018-01-01"),-c(1,2)]/100
table(Sondaggi$Istituto)
SWG <- Sondaggi[which(Sondaggi$Istituto %in% c("SWG","POLITICHE 2018")),]

#prendiamo solamente i dati di un'agenzia 
SWG <- SWG[order(SWG$Data),]

#PRIMO GRAFICO
a <- ts(SWG$PD, 
        freq=365.25/7, 
        start=decimal_date(ymd("2017-01-08")))
library(xts)
PD <- xts(SWG$PD,order.by=as.POSIXct(SWG$Data))
FI <- xts(SWG$`Forza Italia`,order.by=as.POSIXct(SWG$Data))
LEGA <- xts(SWG$Lega,order.by=as.POSIXct(SWG$Data))
M5s <- xts(SWG$M5S,order.by=as.POSIXct(SWG$Data))

timese <- cbind(PD=PD,FI=FI,LEGA=LEGA,M5S=M5s)
plot(a)
library(dygraphs)

dyHide <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Hide",
    path = system.file("plugins/hide.js", package = "dygraphs")
  )
}

dyUnzoom <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}


dygraph(timese, main= "Sondaggi Elettorali") %>% dySeries("PD", color = "red" ,label = "PD") %>%
  dySeries("FI", label = "Forza Italia",color = "blue") %>% 
  dySeries("LEGA", label = "Lega",color="green") %>% dySeries("M5S", label = "M5S",color="gold") %>%
  dyRangeSelector() %>% 
dyShading(from = "2018-02-14", to = "2018-03-04", color = "#FFE6E6") %>% 
dyAnnotation("2018-03-04", text = "EP", tooltip = "Elezioni Politiche") %>%
  dyCSS("dygraph.css") %>% 
  dyHide() %>% 
  dyUnzoom()

#SECONDO GRAFICO (PLOTLY) UTILIZZATO POI NEL BLOG
library(plotly)
# plot_ly(SWG, x = ~Data, y = ~PD, name="PD",
#         type = "scatter", mode = "lines") %>%
#   add_trace(y= ~Lega, name="Lega", type = "scatter",selected=F) %>%
#   add_trace(y= ~`Forza Italia`, name="Forza Italia", type = "scatter",selected=F) %>%
#   add_trace(y= ~M5S, name="M5S", type = "scatter",unselected=T) %>%
#   add_trace(y= ~FDI, name="FDI", type = "scatter") %>%
#   add_trace(y= ~LeU, name="Liberi e Uguali", type = "scatter",selected=F) %>%
#   layout(yaxis = list(title = "Perc")) 

hovertext1 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$PD, "</b><br>")
hovertext2 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$Lega, "</b><br>")
hovertext3 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$M5S, "</b><br>")
hovertext4 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$`Forza Italia`, "</b><br>")
hovertext5 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$FDI, "</b><br>")
hovertext6 <- paste0("Date:<b>", SWG$Data, "</b><br>",
                     "Perc:<b>", SWG$LeU, "</b><br>")



p <- plot_ly(data=SWG, x = ~Data) %>% 
  add_lines(y = ~PD, line = list(color = "#ff8080", width = 2),
            hoverinfo = "text", text = hovertext1, name = "PD") %>% 
  add_lines(y = ~Lega, line = list(color = "#009933", width = 2),
            hoverinfo = "text", text = hovertext2, name = "Lega") %>% 
  add_lines(y = ~M5S, line = list(color = "#ffcc00", width = 2),
            hoverinfo = "text", text = hovertext3, name = "M5S") %>% 
  add_lines(y = ~`Forza Italia`, line = list(color = "#66ccff", width = 2),
            hoverinfo = "text", text = hovertext4, name = "FI") %>% 
  add_lines(y = ~FDI, line = list(color = "#0000ff", width = 2),
            hoverinfo = "text", text = hovertext5, name = "FDI") %>% 
  add_lines(y = ~LeU, line = list(color = "#cc0000", width = 2),
            hoverinfo = "text", text = hovertext6, name = "LeU") %>% 
  add_markers(x = "2018-03-04",
              y = 0.35,             
              marker = list(size = 15, color = "#00526d"),
              showlegend = F,name="Politiche") %>% 
  add_text(x = "2018-03-04",
           y = 0.35,
           text = "<b>1</b>",
           textfont = list(color = "white", size = 8),
           showlegend = F) %>%
  #add_lines(x="2018-03-04",y=(seq(0,0.35,by=0.05)),hoverinfo=F,line = list(color = "black", width = 2)
   #         ,showlegend=F,hoverinfo="none") %>% 
  add_markers(x = "2017-01-10",
              y = 0.37,             
              marker = list(size = 15, color = "#00526d"),
              showlegend = F,name="Politiche") %>% 
  add_text(x = "2017-01-10",
           y = 0.37,
           text = "<b>1</b>",
           textfont = list(color = "white", size = 8),
           showlegend = F) %>% 
  layout(yaxis = list(title = "Perc"),
         #title="<b>Andamento sondaggi politici 2017-2018</b>",
         annotations =   list(list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                             x = "2017-01-18", y = 0.37, showarrow = F,
                             align = "left",
                             text = "<b>Elezioni Politiche 2018</b>",
                             font = list(size = 12, family = "Arial"),
                             bgcolor = "white"),
                               list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                                    x = 0, y = 1.05, showarrow = F,
                                    text = "<b>Andamento sondaggi politici 2017-2018</b>",
                                    font = list(size = 30, family = "Balto")))
         #paper_bgcolor= "#e6e6ff"
         ) 

# p %>% htmlwidgets::onRender("
#     function(el, x) {
#       // when hovering over an element, do something
#       el.on('plotly_hover', function(d) {
# 
#         // extract tooltip text
#         txt = d.points[0].data.text;
#         // image is stored locally
#         image_location = 'D:/Universita/Statistical Learning/Progetto/Partiti/'+ text + '.jpg';
# 
#         // define image to be shown
#         var img = {
#           // location of image
#           source: image_location,
#           // top-left corner
#           x: 0,
#           y: 1,
#           sizex: 0.2,
#           sizey: 0.2,
#           xref: 'paper',
#           yref: 'paper''
#         };
# 
#         // show image and annotation 
#         Plotly.relayout(el.id, {
#             images: [img] 
#         });
#       })
#     }
#     ")
# 





