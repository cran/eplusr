# GEOMETRY EXTRACTION {{{
test_that("Geometry Extraction", {
    skip_on_cran()

    example <- copy_example(LATEST_EPLUS_VER)
    idf <- read_idf(example$idf)

    expect_equal(get_geom_class(idf),
        data.table(
            category = c("Surface", "SubSurface", "Shading"),
            class = c("BuildingSurface:Detailed", "FenestrationSurface:Detailed", "Shading:Zone:Detailed"),
            type = c("BuildingSurface", "FenestrationSurface", "Shading"),
            subtype = c("Detailed", "Detailed", "Zone"),
            misc = c("", "", "Detailed")
        )
    )

    expect_equal(get_geom_class(idf, 80),
        data.table(
            category = c("Surface"),
            class = c("BuildingSurface:Detailed"),
            type = c("BuildingSurface"),
            subtype = c("Detailed"),
            misc = c(""),
            id = 80L, name = "WALL-1PF"
        )
    )

    # global geometry rules
    expect_equal(get_global_geom_rules(idf),
        list(starting_vertex_position = "upperleftcorner",
             vertex_entry_direction = "counterclockwise",
             coordinate_system = "relative",
             daylighting_reference_point_coordinate_system = "relative",
            rectangular_surface_coordinate_system = "relative"
        )
    )
    without_checking(idf$GlobalGeometryRules$Coordinate_System <- "Absolute")
    expect_equal(get_global_geom_rules(idf)$coordinate_system, "world")
    ## can handle malformed global geometry rules
    without_checking(idf$GlobalGeometryRules$set(..3 = NULL, ..4 = NULL, ..5 = NULL, .default = FALSE, .empty = TRUE))
    suppressWarnings(expect_warning(rules <- get_global_geom_rules(idf)))
    expect_equal(unlist(rules, FALSE, FALSE)[3:5], rep("relative", 3))
    without_checking(idf$GlobalGeometryRules$set(..3 = "Invalid", ..4 = "Invalid", ..5 = "Invalid", .default = FALSE, .empty = TRUE))
    suppressWarnings(expect_warning(rules <- get_global_geom_rules(idf)))
    expect_equal(unlist(rules, FALSE, FALSE)[3:5], rep("relative", 3))

    # building transformation
    expect_equal(get_building_transformation(idf),
        list(id = 3L, name = "Building", north_axis = 30)
    )
    ## can handle malformed building object
    without_checking(idf$Building$North_Axis <- NULL)
    expect_warning(expect_equal(get_building_transformation(idf)$north_axis, 0))

    # zone transformation
    expect_equal(d <- get_zone_transformation(idf),
        data.table(
            id = 74:79,
            name = c("PLENUM-1", "SPACE1-1", "SPACE2-1", "SPACE3-1", "SPACE4-1", "SPACE5-1"),
            x = 0, y = 0, z = 0, dir_relative_north = 0
        )
    )

    # spaces
    expect_equal(get_spaces(read_idf(example$idf)),
        data.table(id = integer(), name = character(), type = character(), zone = character())
    )
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "5ZoneAirCooledWithSpaces.idf"))
    expect_s3_class(spaces <- get_spaces(idf), "data.table")
    expect_equal(names(spaces), c("id", "name", "type", "zone"))
    expect_equal(nrow(spaces), 8L)

    # detailed surface
    idf <- read_idf(example$idf)
    expect_type(surf_d <- extract_geom_surface_detailed(idf), "list")
    expect_equal(names(surf_d), c("meta", "vertices"))
    expect_equal(nrow(surf_d$meta), 40L)
    expect_equal(names(surf_d$meta),
        c("id", "name", "class", "surface_type", "construction_name",
          "space_name", "zone_name", "outside_boundary_condition",
          "outside_boundary_condition_object", "sun_exposure", "wind_exposure")
    )
    expect_equal(names(surf_d$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(surf_d$vertices), 160L)
    expect_type(surf_d <- extract_geom_surface_detailed(idf, object = 80L), "list")
    expect_equal(nrow(surf_d$meta), 1L)
    expect_equal(nrow(surf_d$vertices), 4L)

    # detailed subsurface
    expect_type(subsurf_d <- extract_geom_subsurface_detailed(idf), "list")
    expect_equal(names(subsurf_d), c("meta", "vertices"))
    expect_equal(nrow(subsurf_d$meta), 6L)
    expect_equal(names(subsurf_d$meta),
        c("id", "name", "class", "surface_type", "construction_name",
          "building_surface_name", "outside_boundary_condition_object")
    )
    expect_equal(names(subsurf_d$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(subsurf_d$vertices), 24L)
    expect_type(subsurf_d <- extract_geom_subsurface_detailed(idf, object = 120), "list")
    expect_equal(nrow(subsurf_d$meta), 1L)
    expect_equal(nrow(subsurf_d$vertices), 4L)

    # detailed shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "HospitalLowEnergy.idf"))
    expect_type(shade_d <- extract_geom_shading_detailed(idf), "list")
    expect_equal(names(shade_d), c("meta", "vertices"))
    expect_equal(nrow(shade_d$meta), 25L)
    expect_equal(names(shade_d$meta), c("id", "name", "class", "surface_type", "base_surface_name"))
    expect_equal(names(shade_d$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(shade_d$vertices), 100L)
    expect_type(shade_d <- extract_geom_shading_detailed(idf, object = "MOB Floor 5 Perimeter 2 Ext Wall:shading_south_1"), "list")
    expect_equal(nrow(shade_d$meta), 1L)
    expect_equal(nrow(shade_d$vertices), 4L)

    # daylighting points
    expect_type(dayl_pnts <- extract_geom_daylighting_point(idf), "list")
    expect_equal(names(dayl_pnts), c("meta", "vertices"))
    expect_equal(nrow(dayl_pnts$meta), 48L)
    expect_equal(names(dayl_pnts$meta), c("id", "name", "class", "zone_name"))
    expect_equal(names(dayl_pnts$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(dayl_pnts$vertices), 48L)
    expect_type(dayl_pnts <- extract_geom_daylighting_point(idf, object = "MOB Floor 5 Perimeter 4_DaylRefPt1"), "list")
    expect_equal(nrow(dayl_pnts$meta), 1L)
    expect_equal(nrow(dayl_pnts$vertices), 1L)

    # simple surfaces
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    expect_type(surf_s <- extract_geom_surface_simple(idf), "list")
    expect_equal(names(surf_s), c("meta", "vertices"))
    expect_equal(nrow(surf_s$meta), 24L)
    expect_equal(names(surf_s$meta),
        c("id", "name", "class", "surface_type", "construction_name",
          "space_name", "zone_name", "outside_boundary_condition",
          "outside_boundary_condition_object", "sun_exposure", "wind_exposure")
    )
    expect_equal(names(surf_s$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(surf_s$vertices), 96L)
    expect_type(surf_s <- extract_geom_surface_simple(idf, object = 86), "list")
    expect_equal(nrow(surf_s$meta), 1L)
    expect_equal(nrow(surf_s$vertices), 4L)

    # simple subsurface
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    idf$add(
        Wall_Exterior = list("wall", "EXTERIOR", "ZONE 4", ..5 = 0),
        Window = list("win", "SINGLE PANE HW WINDOW", "wall", NULL, 1, 8, 3, 3, 5),
        Window = list("win2", "SINGLE PANE HW WINDOW", "Zn004:Wall003", NULL, 1, NULL, 3, 3, 5)
    )
    expect_type(subsurf_s <- extract_geom_subsurface_simple(idf), "list")
    expect_equal(names(subsurf_s), c("meta", "vertices"))
    expect_equal(nrow(subsurf_s$meta), 11L)
    expect_equal(names(subsurf_s$meta),
        c("id", "name", "class", "surface_type", "construction_name",
          "building_surface_name", "outside_boundary_condition_object")
    )
    expect_equal(names(subsurf_s$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(subsurf_s$vertices), 44L)
    expect_type(subsurf_s <- extract_geom_subsurface_simple(idf, object = 84), "list")
    expect_equal(nrow(subsurf_s$meta), 1L)
    expect_equal(nrow(subsurf_s$vertices), 4L)

    # simple shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    without_checking(idf$add(
        Shading_Overhang_Projection = list("overhang1", NULL, 0.7, 90, 0, 0, 0.5),
        Shading_Overhang_Projection = list("overhang2", "Zn001:Wall001:Win001", NULL, NULL, 0, 0, NULL),
        Shading_Fin_Projection = list("fin1", "win", 0.1, 0.1, 0.1, 90, 3, 0.1, 0.1, 0.1, 90, 3),
        Shading_Fin_Projection = list("fin2", "Zn001:Wall001:Win001", NULL, NULL, 0.1, 90, 3, NULL, NULL, 0.1, 90, 3)
    ))
    expect_type(shade_s <- extract_geom_shading_simple(idf), "list")
    expect_equal(names(shade_s), c("meta", "vertices"))
    expect_equal(nrow(shade_s$meta), 18L)
    expect_equal(names(shade_s$meta), c("id", "name", "class", "surface_type", "base_surface_name"))
    expect_equal(names(shade_s$vertices), c("id", "index", "x", "y", "z"))
    expect_equal(nrow(shade_s$vertices), 72L)

    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    expect_warning(expect_type(geom <- extract_geom(idf), "list"))
    expect_equal(names(geom), c("rules", "building", "zone", "space", "surface", "subsurface", "shading", "daylighting_point", "vertices"))
})
# }}}

# SIMPLE GEOMETRY CONVERSION {{{
test_that("Simple Geometry Conversion", {
    skip_on_cran()

    # simple shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    cls <- get_geom_class(idf)

    expect_type(geom <- convert_geom_surface_simple(idf), "list")
    expect_equal(names(geom), c("object", "value", "map"))
    expect_equal(nrow(geom$object), 24L)
    expect_equal(nrow(geom$value), 552L)
    expect_equal(nrow(geom$map), 24L)

    expect_type(geom <- convert_geom_subsurface_simple(idf), "list")
    expect_equal(names(geom), c("object", "value", "map"))
    expect_equal(nrow(geom$object), 9L)
    expect_equal(nrow(geom$value), 189L)
    expect_equal(nrow(geom$map), 9L)

    expect_type(geom <- convert_geom_shading_simple(idf), "list")
    expect_equal(names(geom), c("object", "value", "map"))
    expect_equal(nrow(geom$object), 12L)
    expect_equal(nrow(geom$value), 188L)
    expect_equal(nrow(geom$map), 12L)

    expect_warning(expect_type(convert_geom(idf), "list"))
    expect_true(all(!cls$class %chin% get_geom_class(idf)$class))
})
# }}}

# ALIGN COORDINATE SYSTEM {{{
test_that("Align coordinate system", {
    skip_on_cran()

    # simple shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    cls <- get_geom_class(idf)

    expect_warning(expect_type(geoms <- extract_geom(idf), "list"))
    expect_type(geoms <- align_coord_system(geoms), "list")
    expect_type(geoms <- align_coord_system(geoms, "relative", "relative", "relative"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("relative", 3L))
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
    expect_equal(unlist(geoms$rules[3:5], FALSE, FALSE), rep("world", 3L))

    suppressWarnings(expect_warning(geoms <- extract_geom(empty_idf(LATEST_EPLUS_VER))))
    expect_type(geoms <- align_coord_system(geoms, "world", "world", "world"), "list")
})
# }}}

# GEOMETRY SUBSET {{{
test_that("subset_geom", {
    skip_on_cran()

    # simple shading
    idf <- read_idf(path_eplus_example(LATEST_EPLUS_VER, "4ZoneWithShading_Simple_1.idf"))
    expect_warning(expect_type(geoms <- extract_geom(idf), "list"))
    expect_type(l <- subset_geom(geoms, "all"), "list")
    expect_equal(nrow(l$surface), 24L)
    expect_equal(nrow(l$subsurface), 9L)
    expect_equal(nrow(l$shading), 12L)
    expect_equal(nrow(l$vertices), 180L)

    expect_type(l <- subset_geom(geoms, "floor"), "list")
    expect_equal(nrow(l$surface), 4L)
    expect_equal(nrow(l$subsurface), 0L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 16L)

    expect_type(l <- subset_geom(geoms, "roof"), "list")
    expect_equal(nrow(l$surface), 4L)
    expect_equal(nrow(l$subsurface), 0L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 16L)

    expect_type(l <- subset_geom(geoms, "wall"), "list")
    expect_equal(nrow(l$surface), 16L)
    expect_equal(nrow(l$subsurface), 0L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 64L)

    expect_type(l <- subset_geom(geoms, c("wall", "window")), "list")
    expect_equal(nrow(l$surface), 16L)
    expect_equal(nrow(l$subsurface), 8L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 96L)

    expect_type(l <- subset_geom(geoms, zone = "zone 1"), "list")
    expect_equal(nrow(l$surface), 6L)
    expect_equal(nrow(l$subsurface), 2L)
    expect_equal(nrow(l$shading), 4L)
    expect_equal(nrow(l$vertices), 48L)

    path_space <- path_eplus_example(LATEST_EPLUS_VER, "5ZoneAirCooledWithSpaces.idf")
    expect_type(l <- subset_geom(extract_geom(read_idf(path_space)), space = "space 3 open office 1"), "list")
    expect_equal(nrow(l$zone), 1L)
    expect_equal(nrow(l$space), 1L)
    expect_equal(nrow(l$surface), 6L)
    expect_equal(nrow(l$subsurface), 1L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 28L)

    expect_type(l <- subset_geom(geoms, surface = "Zn001:Wall001"), "list")
    expect_equal(nrow(l$surface), 1L)
    expect_equal(nrow(l$subsurface), 0L)
    expect_equal(nrow(l$shading), 0L)
    expect_equal(nrow(l$vertices), 4L)
})
# }}}

# vim: set fdm=marker:
