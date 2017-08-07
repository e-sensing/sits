library(sits)

# Read data 
data.tb <- sits_getdata(file = './inst/extdata/samples/cerrado.json')

# Set seed for reproducibility 
set.seed(1986)

# Train machine learn models 
train.tb <- sits_train(data.tb, span = 0, bands = c('ndvi', 'evi'))

# Classify time series 
res.tb <- sits_predict(data.tb, model.fit = train.tb)

# Classification results 
res.tb %>% 
     dplyr::select(classification)

