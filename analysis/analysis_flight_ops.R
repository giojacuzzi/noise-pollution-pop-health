data = read.csv('data/Noise Modeling Data/Aggregates/NASWI - Aggregate Flight Operations MP3.csv')

# Most active ops total
n = 20
data[order(-data$Num.Total)[1:n], c('Num.Total', 'Track.Group', 'Track.Type', 'Track', 'Runway', 'A.C.Category', 'Profile')]

# Most active track types and groups
levels(factor(data$Track.Type))
ops_per_track_type  = round(tapply(X=data$Num.Total, INDEX=data$Track.Type, FUN=sum), 2)
ops_per_track_group = round(tapply(X=data$Num.Total, INDEX=data$Track.Group, FUN=sum), 2)
track_groups = unlist(labels(ops_per_track_group))
df_track = data.frame(
  Track.Group = track_groups,
  Track.Type  = data[match(track_groups, data$Track.Group), 'Track.Type'],
  Num.Total   = ops_per_track_group
)
rownames(df_track) = c()
df_track = df_track[order(df_track$Num.Total, decreasing=T), ]

ggplot(data, aes(x=Num.Total, y=Track.Type)) + 
  geom_bar(stat = 'identity')

ggplot(data, aes(x=Num.Total, y=Track.Group, fill=Track.Type)) + 
  geom_bar(stat = 'identity')

# Track activity
round(tapply(X=data$Num.Total, INDEX=data$Track, FUN=sum), 2)

# Runway activity
round(tapply(X=data$Num.Total, INDEX=data$Runway, FUN=sum), 2)
