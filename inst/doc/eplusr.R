## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    screenshot.force = FALSE
)

# the default output hook
hook_output = knitr::knit_hooks$get('output')
knitr::knit_hooks$set(output = function(x, options) {
    if (!is.null(n <- options$out.lines)) {
        x <- unlist(strsplit(x, '\n', fixed = TRUE))
        if (length(x) > n) {
            # truncate the output
            x <- c(head(x, n), '....', '')
        } else {
            x <- c(x, "")
        }
        x <- paste(x, collapse = '\n') # paste first n lines together
    }
    hook_output(x, options)
})

knitr::opts_knit$set(root.dir = tempdir())

options(crayon.enabled = FALSE)

can_run <- eplusr:::os_type() != "unknown"

## ----cran-install, eval = FALSE------------------------------------------
#  install.packages("eplusr")

## ----gh-installation, eval = FALSE---------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("hongyuanjia/eplusr")

## ----eplus-install, eval = FALSE-----------------------------------------
#  # install the latest version (currently v9.1.0)
#  eplusr::install_eplus("latest")
#  
#  # OR download the latest version (currently v9.1.0) and run the installer
#  # manually by yourself
#  eplusr::download_eplus("latest", dir = tempdir())

## ----install_eplus, include = FALSE, eval = can_run----------------------
# download portable EnergyPlus
if (!eplusr::is_avail_eplus(8.8)) {
    binary_url <- eplusr:::eplus_download_url(8.8)
    if (eplusr:::is_windows()) {
        ext <- ".zip"
    } else {
        ext <- ".tar.gz"
    }
    port_url <- paste0(tools::file_path_sans_ext(binary_url), ext)
    dest <- file.path(tempdir(), basename(port_url))
    dl <- eplusr:::download_file(port_url, dest)
    eplus_dir <- file.path(tools::file_path_sans_ext(basename(binary_url)), "EnergyPlus-8-8-0")
    if (eplusr:::is_windows()) {
        unzip(dest, exdir = tempdir())
    } else {
        untar(dest, exdir = tempdir())
    }
    eplusr::use_eplus(file.path(tempdir(), eplus_dir))
}

## ---- results = "asis", echo = FALSE, eval = can_run, include = can_run----
cat('
<p align="center">
  <img src="../man/figures/class_structure.png"/>
</p>
')

## ----copy_example, include = FALSE, eval = can_run-----------------------
library(eplusr)

cfg <- eplus_config(8.8)

example_name <- "5Zone_Transformer.idf"
weather_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
ddy_name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"

path_example <- file.path(cfg$dir, "ExampleFiles", example_name)
path_weather <- file.path(cfg$dir, "WeatherData", weather_name)
path_ddy <- file.path(cfg$dir, "WeatherData", ddy_name)

file.copy(path_example, tempdir(), overwrite = TRUE)
file.copy(c(path_weather, path_ddy),
  file.path(tempdir(), c("San_Francisco.epw", "San_Francisco.ddy")), overwrite = TRUE)

## ----idd_dl, eval = FALSE------------------------------------------------
#  path_idd <- download_idd(8.8)
#  use_idd(path_idd)
#  
#  # OR
#  use_idd(8.8, download = TRUE)

## ----idd_solaris, include = FALSE, eval = !can_run-----------------------
#  library(eplusr)
#  use_idd(8.8, download = TRUE)
#  path_example <- "https://raw.githubusercontent.com/NREL/EnergyPlus/v8.8.0/testfiles/5Zone_Transformer.idf"
#  path_weather <- "https://raw.githubusercontent.com/NREL/EnergyPlus/v8.8.0/weather/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#  path_ddy <- "https://raw.githubusercontent.com/NREL/EnergyPlus/v8.8.0/weather/USA_CA_San.Francisco.Intl.AP.724940_TMY3.ddy"
#  
#  eplusr:::download_file(path_example, file.path(tempdir(), basename(path_example)))
#  eplusr:::download_file(path_weather, file.path(tempdir(), "San_Francisco.epw"))
#  eplusr:::download_file(path_ddy, file.path(tempdir(), "San_Francisco.ddy"))

## ----read, out.lines = 10------------------------------------------------
model <- read_idf(path = "5Zone_Transformer.idf", idd = NULL)

model

## ----all_grp, out.lines = 5----------------------------------------------
model$group_name()

## ----all_cls, out.lines = 5----------------------------------------------
model$class_name()

## ----chk_grp-------------------------------------------------------------
model$is_valid_group("Schedules")
model$is_valid_class("ZoneInfiltration:DesignFlowRate")

## ----all_field-----------------------------------------------------------
def_mat <- model$definition("Material")
def_mat

## ----idd_obj, eval = FALSE-----------------------------------------------
#  idd <- use_idd(8.8)
#  idd$Material
#  
#  # OR
#  # idd$object("Material")
#  
#  # OR
#  # idd_object(8.8, "Material")

## ----mat_def-------------------------------------------------------------
def_val <- def_mat$field_default()
str(def_val)

## ----all_id--------------------------------------------------------------
model$object_id(class = c("Material", "Construction"), simplify = FALSE)

## ----obj_nm--------------------------------------------------------------
model$object_name(class = c("Version", "Material", "Construction"), simplify = FALSE)

## ----obj_num-------------------------------------------------------------
model$object_num(c("BuildingSurface:Detailed", "Material", "Output:Variable"))

## ----obj-----------------------------------------------------------------
model$objects(c("WD10", "ROOF-1"))

## ----obj_in_cls, out.lines = 30------------------------------------------
model$objects_in_class("Material")

## ----obj_in_cls_shortcut_1, out.lines = 30-------------------------------
model$Material_NoMass
# OR
# model[["Material_NoMass"]]

## ----rp------------------------------------------------------------------
rp <- model$RunPeriod[[1]]

## ----objuni--------------------------------------------------------------
model$object_unique("Building")

# OR just
# model$Building

## ----rel-----------------------------------------------------------------
model$object_name("Material:NoMass")
model$object_relation("mat-clng-1")

## ----obj_rel-------------------------------------------------------------
mat_const <- model$objects_in_relation("mat-clng-1", "ref_by")
mat_const

## ----s3_obj--------------------------------------------------------------
rp$Begin_Day_of_Month

# OR
# rp[["Begin_Day_of_Month"]]
# rp[[3]]

## ----chain---------------------------------------------------------------
model$RunPeriod$WinterDay$Begin_Day_of_Month

## ----dup-----------------------------------------------------------------
model$dup(c(my_roof = "ROOF-1", "ROOF-1", "WALL-1"))

## ----add_obj-------------------------------------------------------------
rp1 <- list(RunPeriod = list("rp_test_1", 1, 1, 2, 1, .comment = c("Comment for new object 1", "Another comment")))

model$add(rp1,
  RunPeriod = list(name = "rp_test_2", begin_month = 3, begin_day_of_month = 1,
    end_month = 4, end_day_of_month = 1, .comment = "Comment for new object 2"
  )
)

## ----set_obj-------------------------------------------------------------
model$set(
  rp_test_1 = list(name = "rp_test_3", begin_day_of_month = 2,
    .comment = c(format(Sys.Date()), "begin day has been changed.")
  )
)

## ----set_chain-----------------------------------------------------------
model$RunPeriod$rp_test_2$End_Day_of_Month <- 2
model$RunPeriod$rp_test_2$End_Day_of_Month

## ----set_ref-------------------------------------------------------------
mat <- model$Material$CC03

mat$value_relation("Name")

mat$set(name = "CC03_renamed")

mat$value_relation("Name")

## ----possible------------------------------------------------------------
mat$value_possible(c(2, 7))

## ----ddy, warning=TRUE, out.lines = 20-----------------------------------
# read ddy file as normal IDF
ddy <- read_idf("San_Francisco.ddy", idd = 8.8)

model$insert(ddy$SizingPeriod_DesignDay)

# get location data
loc <- ddy$Site_Location$value()

model$Site_Location$set(loc)


## ----load_chr------------------------------------------------------------
mat_chr <- c("Construction,", "new_const1,", paste0(model$Material[[1]]$name(), ";"))
model$load(mat_chr)

# extract first construction data in a data.table
dt <- model$Construction[[1L]]$to_table()
# modify value
dt[1, value := "new_const2"]
model$load(dt)

## ------------------------------------------------------------------------
model$object_relation("new_const1")
model$object_relation("new_const2")

## ----ref_by--------------------------------------------------------------
model$Material_NoMass$`MAT-CLNG-1`$value_relation()

## ----del, error = TRUE---------------------------------------------------
model$del("mat-clng-1")

## ----del_1---------------------------------------------------------------
model$del("mat-clng-1", .force = TRUE)

## ----validate------------------------------------------------------------
eplusr_option("validate_level")
str(level_checks("final"))

## ----valid---------------------------------------------------------------
model$validate(custom_validate(reference = TRUE))

## ------------------------------------------------------------------------
(id <- model$validate()$invalid_reference$object_id)
model$objects(id)

## ------------------------------------------------------------------------
model$object(id)$value_possible("Outside Layer")$source

## ------------------------------------------------------------------------
model$object(id)$set(Outside_Layer = "WD10")

## ----save, eval = FALSE--------------------------------------------------
#  model$save(overwrite = TRUE)
#  
#  model$save("test.idf")

## ----avail_eplus---------------------------------------------------------
avail_eplus()

## ----use_eplus, eval = FALSE---------------------------------------------
#  use_eplus("C:/EnergyPlusV8-8-0")

## ----install, eval = FALSE-----------------------------------------------
#  install_eplus(ver = 8.8)

## ----run, eval = can_run, out.lines = 10---------------------------------
# read the model again
model <- read_idf("5Zone_Transformer.idf")
path_epw <- "San_Francisco.epw"

job <- model$run(path_epw, wait = TRUE)

## ----errors, eval = can_run----------------------------------------------
job$errors()

## ----dict, eval = can_run------------------------------------------------
str(job$report_data_dict())

## ----output, eval = can_run----------------------------------------------
power <- job$report_data("transformer 1", "transformer input electric power", case = "example",
  all = TRUE, simulation_days = 1, environment_name = "summerday", hour = 11, minute = 0)

str(power)

## ----tab, eval = can_run-------------------------------------------------
site_energy <- job$tabular_data(column_name = "energy per total building area", row_name = "total site energy")
site_energy[, value := as.numeric(value)]
str(site_energy)

## ----del_job, include = FALSE, eval = can_run----------------------------
clean_wd(model$path())

## ----param, eval = can_run-----------------------------------------------
param <- param_job(idf = model, epw = path_epw)

param

## ----mea, eval = can_run-------------------------------------------------
set_infil_rate <- function (idf, infil_rate) {

  # validate input value
  # this is optional, as validations will be made when setting values to `Idf`
  stopifnot(is.numeric(infil_rate), infil_rate >= 0)

  if (!idf$is_valid_class("ZoneInfiltration:DesignFlowRate"))
    stop("Input model does not have any object in class `ZoneInfiltration:DesignFlowRate`")

  # get all object IDS
  ids <- idf$object_id("ZoneInfiltration:DesignFlowRate", simplify = TRUE)

  # make a list of new values to set
  new_val <- list(design_flow_rate_calculation_method = "AirChanges/Hour", air_changes_per_hour = infil_rate)

  # create proper format for all objects in that class
  val <- rep(list(new_val), length(ids))
  names(val) <- paste0("..", ids)

  idf$set(val)

  idf
}

## ----apply, eval = can_run-----------------------------------------------
param$apply_measure(set_infil_rate, seq(0, 4, by = 1), .names = NULL)

## ----param_run, eval = can_run-------------------------------------------
param$run(wait = TRUE)

## ----param_res, eval = can_run-------------------------------------------
tab <- param$tabular_data(
  table_name = "Site and Source Energy",
  column_name = "Total Energy",
  row_name = "Total Site Energy"
)

total_eng <- tab[, list(case, `Total Energy (GJ)` = as.numeric(value))]

## ----eval = FALSE--------------------------------------------------------
#  total_eng

## ----echo = FALSE, results="asis", eval = can_run------------------------
knitr::kable(total_eng)

## ----del_param, include = FALSE, eval = can_run--------------------------
dir_nms <- paste0("set_infil_rate_", 1:5)
lapply(dir_nms, unlink, recursive = TRUE, force = TRUE)

## ----clean_files, include = FALSE, eval = can_run------------------------
unlink(file.path(tempdir(), c(example_name, "San_Francisco.epw", "San_Francisco.ddy")))

