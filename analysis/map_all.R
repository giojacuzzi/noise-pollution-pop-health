# Run all the other scripts first
mapview(get_flighttracks()) +
  mapview(pop_exposure_stack[['Ldn']], layer.name=c('Ldn (dB)')) +
  mapview(pop_exposure_stack[['Impacted.Population']], layer.name=c('Impacted Persons')) +
  mapview(estimated_pop_HA_WHO, layer.name=c('Persons Highly Annoyed (WHO)')) + # at=seq(0,5,1)
  mapview(estimated_pop_HA_Yokoshima, layer.name=c('Persons Highly Annoyed (Yokoshima)')) + # at=seq(0,5,1)
  mapview(pop_exposure_stack[['Lnight']], layer.name=c('Lnight (dB)')) +
  mapview(estimated_pop_HSD, layer.name=c('Persons Highly Sleep Disturbed (WHO)')) +
  mapview(pop_exposure_stack[['Leq24']], layer.name=c('Leq24 (dB)')) +
  mapview(estimated_pop_hearing_loss, layer.name=c('Persons Hearing Loss (EPA)')) +
  mapview(schools, zcol='Ldn55', col.regions=list('gray', 'red'), layer.name=c('Impacted Schools'))
