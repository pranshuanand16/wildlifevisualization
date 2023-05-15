library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(jsonlite)


data <- fromJSON('https://sheetdb.io/api/v1/jvxmtjioo1ess')
head(data)

df <- as.data.frame(data)
class(df)
head(df)

df<- transform(df,Longitude = as.numeric(Longitude),Latitude = as.numeric(Latitude))

map <- leaflet(df)



tiger_icon <- makeIcon(
  
  iconUrl = "tigerff.png",
  iconWidth = 60 ,
  iconHeight = 60,
  iconAnchorX = 60,
  iconAnchorY = 60
)

panda_icon <- makeIcon(
  
  iconUrl = "redpandan.png",
  iconWidth = 30 ,
  iconHeight = 30,
  iconAnchorX = 30,
  iconAnchorY = 30
)

seiwhale_icon <- makeIcon(
  
  iconUrl = "seiwhale.png",
  iconWidth = 70 ,
  iconHeight = 70,
  iconAnchorX = 70,
  iconAnchorY = 70
)
bluewhale_icon <- makeIcon(
  
  iconUrl = "bluewhalen.png",
  iconWidth = 70 ,
  iconHeight = 70,
  iconAnchorX = 70,
  iconAnchorY = 70
)
finwhale_icon <- makeIcon(
  
  iconUrl = "finwhalen.png",
  iconWidth = 70 ,
  iconHeight = 70,
  iconAnchorX = 70,
  iconAnchorY = 70
)
rhino_icon <- makeIcon(
  
  iconUrl = "rhinon.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
wildbuffalo_icon <- makeIcon(
  
  iconUrl = "Wild water buffalo.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
hispidhare_icon <- makeIcon(
  
  iconUrl = "Hispid_hare.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)

sangai_icon <- makeIcon(
  
  iconUrl = "sanagin.png",
  iconWidth = 60 ,
  iconHeight = 60,
  iconAnchorX = 60,
  iconAnchorY = 60
)
nilgiritahr_icon <- makeIcon(
  
  iconUrl = "Niligirn.png",
  iconWidth = 30 ,
  iconHeight = 30,
  iconAnchorX = 30,
  iconAnchorY = 30
)
dhole_icon <- makeIcon(
  
  iconUrl = "dholen.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
elephant_icon <- makeIcon(
  
  iconUrl = "elephantn.png",
  iconWidth = 45 ,
  iconHeight = 45,
  iconAnchorX = 45,
  iconAnchorY = 45
)
macaque_icon <- makeIcon(
  
  iconUrl = "liontailed.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
leafnosedbat_icon <- makeIcon(
  
  iconUrl = "kolar.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
muskdeer_icon <- makeIcon(
  
  iconUrl = "whitebailednn.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
mouseearedbat_icon <- makeIcon(
  
  iconUrl = "Mouse-eared Bat.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
lion_icon <- makeIcon(
  
  iconUrl = "lionn.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
dolphin_icon <- makeIcon(
  
  iconUrl = "dolphinaa.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
langur_icon <- makeIcon(
  
  iconUrl = "langaur.png",
  iconWidth = 60 ,
  iconHeight = 60,
  iconAnchorX = 60,
  iconAnchorY = 60
)
snowleopard_icon <- makeIcon(
  
  iconUrl = "sonowwww.png",
  iconWidth = 60 ,
  iconHeight = 60,
  iconAnchorX = 60,
  iconAnchorY = 60
)
squirrel_icon <- makeIcon(
  
  iconUrl = "flyingsqi.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
stag_icon <- makeIcon(
  
  iconUrl = "kashmiristaga.png",
  iconWidth = 50 ,
  iconHeight = 50,
  iconAnchorX = 50,
  iconAnchorY = 50
)
civet_icon <- makeIcon(
  
  iconUrl = "Malabar large-spotted civet.png",
  iconWidth = 70 ,
  iconHeight = 70,
  iconAnchorX = 70,
  iconAnchorY = 70
)
# Define UI for Shiny app
ui <- fluidPage (
  #titlePanel("Wildlife Locations"),
  fluidRow( leafletOutput("map", width="100%", height= "1000px")),  
  fluidRow( checkboxGroupInput("animals", "Select animals:", inline = TRUE ,choices = unique(df$Species))),
         
)

# Define server function for Shiny app
server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create leaflet map object
    map <- leaflet(df) %>% addTiles() %>% setView(lng = 78.9629, lat = 20.5937, zoom = 4)
    
    #filter data based on selected species
    df_selected <- df%>% filter(Species%in% input$animals)%>%
      mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude))
    
    # Add markers with custom icons based on selected animal
    for(animal in input$animals){
      if (animal == "Bengal tiger") {
        df_temp <- df %>% 
          filter(Species == "Bengal tiger")
        map <- map %>%  addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_selected %>% filter(Species=="Bengal tiger"), lng = ~Longitude, lat = ~Latitude, icon = tiger_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Red panda") {
        df_temp <- df %>% 
          filter(Species == "Red panda") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = panda_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Sei whale") {
        df_temp <- df %>% 
          filter(Species == "Sei whale") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = seiwhale_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Blue whale") {
        df_temp <- df %>% 
          filter(Species == "Blue whale") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = bluewhale_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Fin whale") {
        df_temp <- df %>% 
          filter(Species == "Fin whale") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = finwhale_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Greater one-horned rhinoceros") {
        df_temp <- df %>% 
          filter(Species == "Greater one-horned rhinoceros") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = rhino_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Wild water buffalo") {
        df_temp <- df %>% 
          filter(Species == "Wild water buffalo") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = wildbuffalo_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Hispid hare") {
        df_temp <- df %>% 
          filter(Species == "Hispid hare") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = hispidhare_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Sangai") {
        df_temp <- df %>% 
          filter(Species == "Sangai") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = sangai_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Nilgiri tahr") {
        df_temp <- df %>% 
          filter(Species == "Nilgiri tahr") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = nilgiritahr_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Dhole") {
        df_temp <- df %>% 
          filter(Species == "Dhole") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = dhole_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Indian elephant") {
        df_temp <- df %>% 
          filter(Species == "Indian elephant") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = elephant_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Lion-tailed Macaque") {
        df_temp <- df %>% 
          filter(Species == "Lion-tailed Macaque") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon =macaque_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Kolar Leaf-nosed Bat") {
        df_temp <- df %>% 
          filter(Species == "Kolar Leaf-nosed Bat") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = leafnosedbat_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "White-bellied Musk Deer") {
        df_temp <- df %>% 
          filter(Species == "White-bellied Musk Deer") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = muskdeer_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Mandelli's Mouse-eared Bat") {
        df_temp <- df %>% 
          filter(Species == "Mandelli's Mouse-eared Bat") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = mouseearedbat_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Asiatic Lion") {
        df_temp <- df %>% 
          filter(Species == "Asiatic Lion") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = lion_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Ganges River Dolphin") {
        df_temp <- df %>% 
          filter(Species == "Ganges River Dolphin") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = dolphin_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Gee's Golden Langur") {
        df_temp <- df %>% 
          filter(Species == "Gee's Golden Langur") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = langur_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Snow Leopard") {
        df_temp <- df %>% 
          filter(Species == "Snow Leopard") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = snowleopard_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Namdapha flying squirrel") {
        df_temp <- df %>% 
          filter(Species == "Namdapha flying squirrel") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = squirrel_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Kashmir stag") {
        df_temp <- df %>% 
          filter(Species == "Kashmir stag") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = stag_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      } else if (animal == "Malabar large-spotted civet") {
        df_temp <- df %>% 
          filter(Species == "Malabar large-spotted civet") 
        map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% addMarkers(data=df_temp, lng = ~Longitude, lat = ~Latitude, icon = civet_icon, popup = paste("Species: ", df_temp$Species, "<br>", "Location: ", df_temp$Location, "<br>", "Total Population: ", df_temp$Total.Population, "<br>", "Pincode: ", df_temp$Pincode, "<br>", "State: ", df_temp$State, "<br>", "Category: ", df_temp$Category, "<br>"))
      }}
    
    
    map
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
