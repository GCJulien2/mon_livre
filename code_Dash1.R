# ==========================================================
# POPULATION dans le monde/Annee
# ---------------------------

# Supprime les 16 1er lignes :  skip_empty_rows = 16)
# Transformer les "..." en NA et le type dÃ©cimale
wpp2019_pop = read_delim("data/WPP2019_POP_Population_totale_deux_genre.csv", na="...", locale = locale(decimal_mark = ","))  |> 
  rename("Region_Subregion_Country_area" = "Region, subregion, country or area *") |> 
  select("Region_Subregion_Country_area", 'Type', '1950':78) |> 
  pivot_longer(cols = 3:73,names_to = "annee", values_to = "population") |> 
  mutate(annee = as.numeric(annee))

# ==========================================================
# Nourriture dans le monde/Année
# ---------------------------
food = read_delim("data/Production_Crops_Livestock_E_All_Data.csv") |> 
  select(-ends_with("F")) |> 
  pivot_longer(cols = 'Y1961':'Y2003',names_to = "annee", values_to = "food") |> 
  filter(Area =="World",Element =="Production") |> 
  filter(str_detect(Item,pattern = "Total")) |> 
  mutate(annee = str_remove(annee,'Y')) |> 
  drop_na()

# ==========================================================
# Energie dans le monde/ Année
# ---------------------------
# Je sélectionne les colonnes qui m'intéressent : année, pays, energy
# Je filtre par le mot : World
# Je pivot les colonnes

energy = read_delim("data/owid-energy-data__OWID.csv") |> 
  select("country","year", ends_with("consumption")) |> 
  filter(country == "World")|> 
  pivot_longer(cols = 3:15, names_to = "genre_energy", values_to = "consommation")

# ----------------- > Energie combustible
energy_combustible = energy |> 
  filter( genre_energy!="primary_energy_consumption",
          genre_energy!="fossil_fuel_consumption",
          genre_energy!="low_carbon_consumption",
          genre_energy!="renewables_consumption",
          genre_energy!="other_renewable_consumption") |> 
  drop_na()


# ----------------- > Energie Renouvelable

energy_renouvelable = energy |> 
  filter(genre_energy %in%  c("low_carbon_consumption",
                              "fossil_fuel_consumption",
                              "other_renewable_consumption",
                              "renewables_consumption")) |> 
  drop_na()



#======================================================
#============= FILTRER par Continent ==================

# Ouveture du fichier population 
population_brut = read_delim("data/WPP2019_POP_Population_totale_deux_genre.csv", 
                             na="...", locale = locale(decimal_mark = ","))  |> 
  rename("Region_Subregion" = "Region, subregion, country or area *") |> 
  pivot_longer(cols = '1950':'2020',names_to = "annee", values_to = "population") 

# Creation de l'objet et filtre sur population par continent 
pp_continent = population_brut |> 
  filter(Region_Subregion %in% c("Africa","Asia","Europe","Northern America","South America", "Oceania")) 

#==================================================
# ======================== Nourriture par continent 

countries_continent = read_delim("data/countries_iso_codes.csv", na="...", locale = locale(decimal_mark = ","))

Production_Crops = read_delim("data/Production_Crops_Livestock_E_All_Data.csv") |> 
  select(-ends_with("F")) |> 
  pivot_longer(cols = 'Y1961':'Y2003',names_to = "annee", values_to = "food") |> 
  mutate(annee = str_remove(annee,'Y')) |> 
  filter(Element == "Production")


# Jointure de la table countries_continent & production
# Je sélectionne les colonnes qui m'intéressement 

join_product_continent = left_join(countries_continent, Production_Crops, by = c("name" = "Area")) |> 
  select("region","annee","food") |> 
  group_by(region, annee) |> 
  summarise(food2 = sum(food, na.rm = TRUE) ) |> 
  drop_na()

distinct(join_product_continent, region)

#==================================================
# ======================== Energie par continent 


energy_data_test = read_delim("data/owid-energy-data__OWID.csv") |> 
  select(iso_code, country,year, ends_with("consumption")) |> 
  pivot_longer(cols = 4:16, names_to = "type_energie", values_to = "consommation") |> 
  
  filter( type_energie!="primary_energy_consumption",
          type_energie!="fossil_fuel_consumption",
          type_energie!="low_carbon_consumption",
          type_energie!="renewables_consumption",
          type_energie!="other_renewable_consumption")





#objet = distinct(energy_data,country )
countries_iso_codes = read_delim("data/countries_iso_codes.csv")


join_energy_countrie = left_join(energy_data_test, countries_iso_codes, by = c("iso_code" = "alpha-3")) |>
  select( region, consommation ,year, type_energie) |> 
  group_by(region, year, type_energie) |> 
  summarise(consommation = sum(consommation, na.rm = TRUE) ) |> 
  mutate(type_energie = str_remove(type_energie,'_consumption')) |> 
  drop_na()