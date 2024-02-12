## Flight operations from Olympic MOA
# Data copied from Navy "Report to Congress: Real-Time Aircraft Sound Monitoring Final Report"

m = c(
  'Oct 2020', 'Nov 2020', 'Dec 2020', 'Jan 2021', 'Feb 2021', 'Mar 2021',
  'Apr 2021', 'May 2021', 'Jun 2021', 'Jul 2021', 'Aug 2021', 'Sep 2021', 'Oct 2021'
)

ops_moa = data.frame(
  Month = m,
  EventsDay =    c(135, 269, 248, 253, 344, 321, 235, 274, 313, 277, 318, 241, 216),
  EventsNight =  c(12,  5,   0,   0,   2,   0,   15,  0,   44,  9,   11,  6,   10),
  TotalSorties = c(142, 269, 248, 253, 344, 321, 235, 274, 315, 277, 320, 241, 218)
)

message('Average monthly sorties: ', mean(ops_moa$TotalSorties))
tot_events_day = sum(ops_moa$EventsDay)
tot_events_night = sum(ops_moa$EventsNight)
tot_events = tot_events_day + tot_events_night
message('Percent Day: ', tot_events_day / tot_events)
message('Percent Night: ', tot_events_night / tot_events)
