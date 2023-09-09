library(patchwork)
library(ggsn)
library(OpenStreetMap)
library(osmdata)
library(tigris)
options(tigris_use_cache = T)

# Federal American Indian Reservations, Off-Reservation Trust Lands (ORTL), State American Indian Reservations, Hawaiian Home Lands (HHL), Alaska Native Village Statistical Areas (ANVSA), Oklahoma Tribal Statistical Areas (OTSA), State Designated Tribal Statistical Areas (SDTSA), Tribal Designated Statistical Areas (TDSA), American Indian Joint-Use Areas (AIJUA), Joint-Use Oklahoma Tribal Statistical Areas; January 1, 2023 vintage
native_lands = get_acs(geography = 'american indian area/alaska native area/hawaiian home land', variables = 'B01003_001', year = 2020, geometry = T)
