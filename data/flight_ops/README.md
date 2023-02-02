# Flight operations aggregation

This document details the process for aggregating separate per-period flight operations data files into:
- Aggregated flight operations data per-period
- Total flight operations data (a single comprehensive file aggregating all of the aggregated periods)

The base data ('NASWI_MPX_Y_Noisemap - Flight Operations.xml' files) have been exported from their corresponding .baseops case via BaseOps (all columns), where X is the monitoring period and Y is the file number (for Ault Field; note that Coupeville activity is just represented by a single file per period). These exports are located at `'data/flight_ops/modeling/data/original_exports`. They were then opened in MS Excel, and saved as the .csv files used by the following scripts of this directory:

- `period_aggregate_flight_ops.R`
- `total_aggregate_flight_ops.R`

Run both of these scripts to generate the per-period aggregates and the total singular aggregate csv outputs.

Next, open the aggregate csv file(s) from `data/flight_ops/output` that you want to use with Excel, and re-save as xml file(s) in `data/flight_ops/modeling/data/`.
NOTE: We currently only have day/night values (not evening) for the operations on average annual day, so in the next step, we cannot import any data from an 'evening' column.

Run BaseOps, and open the corresponding .baseops case file from `.../Aggregates/NOISEMAP/`.

To import new flight operations into BaseOps:
- File > Import Flight Operations from Spreadsheet
- Option Categories > File > Import operations from the following spreadsheet file: <the xml file you just saved>
- Option Categories > Columns:
  - Flight Profile Name Column: C (3)
  - Num Day Ops Column: L (12)
  - Num Night Ops Column: M (13)
  - Also import flight tracks: yes
  - Flight Track Name Column: E (5)
- Option Categories > Missing Data
  - If a flight profile in the spreadsheet is missing from the BaseOps case, then... Add the missing profile to the BaseOps case
  - If a flight profile in the BaseOps case is missing from the spreadsheet, then... Leave the profile unchanged in the BaseOps case
  - If you are also importing flight tracks, and a flight track in the spreadsheet is missing from the BaseOps case, then... Set the profile's flight track to "undefined"
- Press OK... You should see the following message:

```
  Importing flight profiles from spreadsheet file NASWI_MP1_Noisemap - Flight Operations.xml
  The following flight profiles appear in both the BaseOps case and the spreadsheet file.

  The daily flight profile operation counts in the BaseOps case will be updated to
  match the values in the spreadsheet file.

  226A_EXP
  226A_FLT
  226A_FRS
  ...
```
Press OK again. You  may need to manually match some flight profile tracks. Then, you can run the case, and plot maps by following instructions in the section below.

---

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
