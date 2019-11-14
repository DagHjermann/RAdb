#
# Used to submit issue on Github
# https://github.com/rstudio/leaflet/issues/653
#

# Fresh install
devtools::install_github('rstudio/leaflet')

# Need reprex
library(reprex)

# Install webshot if necessary
if (!("webshot" %in% installed.packages()[, "Package"])) {
  install.packages("webshot")
  # restart R process to fully integrate with knitr
}


#
# Run the part below
# This makes a markdown text including a picture which is put on Imgur
#
# Ideally, the markdown text should be put on the clipboard for easy copy to Github
# However, replex can't access the clipboard (see output below)
# Instead we store the output as X, and write X to the cliboard instead 
#  (can also check the temporary file written)
#
X <- reprex({
library(leaflet)

outline <- quakes[chull(quakes$long, quakes$lat),]

map <- leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )

map %>% showGroup("Outline")

})

writeLines(X, "clipboard")


# OUTPUT:
#
# Unable to put result on the clipboard. How to get it:
#   * Capture what reprex() returns.
#   * Use `outfile = "foo"` to request output in specific file.
#   * See the temp file:
#   - C:/Users/DHJ/AppData/Local/Temp/RtmpyyQI1k/file5050336a631b_reprex.md


