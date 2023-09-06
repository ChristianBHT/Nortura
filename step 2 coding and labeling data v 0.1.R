library(lubridate)
library(expss)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
load("nortura_raw.Rda")

day_production_data$Dato <- ymd(day_production_data$Dato)

data = apply_labels(data,
                    id_batch = 'Unique ID for chicken batch',
                    id_farmday = 'Unique ID for day batch combination',
                    id_feedfirm = 'Name of producer of growth feed',
                    id_slaughterhouse = 'Name of slaughter',
                    id_hatchery = 'Name of hatchery',
                    prod_type = 'Type of chicken in production',
                    N_of_chicken = 'Number of chickens originally in batch',
                    prod_date = 'Production day date',
                    age = 'Day age of chicken',
                    weight = 'Mean weight of chicken measured',
                    dead_self = 'Chicken died of natural causes',
                    killed_legs = 'Killed due to bad legs',
                    killed_stunted = 'Killed due to stunted growth',
                    killed_pecking = 'Killed due to pecking injuries',
                    killed_other = 'Killed due to other causes',
                    total_dead = 'total number of dead chickens',
                    water_consump = 'daily water consumption',
                    feed_consump = 'daily feed consumption',
                    temp_min = 'daily minimal temperature',
                    temp_max = 'daily maximal temperature',
                    humidity_min = 'daily minimum humidity',
                    humidity_max = 'daily maximum humidity',
                    co2_min = 'daily co2 min',
                    co2_max = 'daily co2 max',
                    light_strength = 'Light strenght in percent',
                    light_hours = 'hours light turned on',
                    id_producer = 'id for producer',
                    date_batch_start = 'date for production start',
                    dead_from_hatchery = 'chickens dead on transport from hatchery',
                    area_prod_house = 'area of the production house',
                    slaughtered = 'number of chickens slaughtered',
                    condition_stunted = 'Discarded at slaugther due to stunted growth when arrived at slaughter house',
                    condition_liver = 'Discarded at slaugther due to liver damage when arrived at slaughter house',
                    condition_legs = 'Discarded at slaugther due to damage on legs',
                    condition_skin = 'Discarded at slaugther due to skin condition',
                    condition_peritoneum = 'Discarded at slaugther due to peritoneum',
                    condition_smell = 'Discarded at slaugther due to smell',
                    condition_heart = 'Discarded at slaugther due to heart conditions',
                    condition_aceties = 'Discarded at slaugther due to aceties',
                    mean_weight = 'Mean weight weighted at slaughter',
                    expected_weight = 'Goal for slaughter weight',
                    primary_feed_growth = 'Brand of primary growth feed',
                    amount_primary_growth = 'Amount bought of primary growth feed (usually everything is eaten)',
                    secondary_feed_growth = 'Brand of primary growth feed',
                    amount_sec_growth = 'Amount bought of primary growth feed (usually everything is eaten)',
                    length = 'Length of production house',
                    width = 'Width of production house',
                    height = 'Height of production house',
                    heating_source = 'Heating source',
                    heating_type = 'Heating type',
                    isolated_floors = 'The floor is isolated'
)


#temperature_data <- read.csv('temperature_data.csv')


save(data,file="nortura_raw_labelled.Rda")








