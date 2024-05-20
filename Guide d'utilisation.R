library(devtools)
install_github("zinebelgoraii/aplication")
library(SDM3)

dataset_path <- "loooooooooop/Datasets/xy.csv"
shapefile_path <- "loooooooooop/Datasets/morocco.shp"
output_folder <- "output4"
input_file <- "E:/aaaaaaaaaaaaa/loooooooooop/Datasets/predictors_masked19.tif"

futur <- "E:/aaaaaaaaaaaaa/data futur"

data_list <- SDM3::data_vis(dataset_path, shapefile_path, output_folder)
train_test_data <- SDM3::data_train(data_list,input_file)

mx<-SDM3::predict_maxent(train_test_data)
rf<-SDM3::predict_rf(train_test_data)
xgb<-SDM3::predict_xgboost(train_test_data)

SDM3::evaluate_models(mx,rf,xgb,output_folder)

prediction_futur(mx,input_file,futur,shapefile_path,output_folder)