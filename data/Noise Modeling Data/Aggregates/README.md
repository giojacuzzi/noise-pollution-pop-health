# Noise contour map generation
Maps are generated with NMap after running a BaseOps case and selecting "Plot"

### Visual options
Edit Options...
- Contours > Levels
  Manually specify contour levels 40-120, spacing 10, num secondary 1
- Contours > Styles
  Primary Contours Line Width 0.4
  Secondary Contours Line Width 0.2
- Contours > Color Gradient
  Manually specify mapping:
  30 white 0
  40 blue 0
  50 green 30
  60 yellow 30
  70 orange 30
  80 red 30
  90 maroon 30
- Geographic Annotations > Points
  Point of Interest Label X
- Background > Layers > Add Layer (SlippyMap) > Options
  Title Server URL Prefix: tile.openstreetmap.org
  Attribution Text: OpenStreetMap
