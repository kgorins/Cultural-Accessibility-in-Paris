# Methodology: static description of sources, pipeline and limitations.

pipeline_steps <- list(
  list("Clean cultural facilities", "Keep Basilic records in the Paris département, select useful fields and normalise whitespace."),
  list("Remove duplicates", "Drop records sharing the same name, address and coordinates."),
  list("Correct invalid coordinates", "Fix a known mis-geocoded facility and check all coordinates fall within Paris."),
  list("Classify facilities", "Group detailed facility types into nine analytical cultural categories."),
  list("Restrict the INSEE grid to Paris", "Keep Filosofi 200 m cells that intersect the Paris boundary (20 arrondissements)."),
  list("Compute cell centroids", "Represent each cell by its centroid (Lambert-93, EPSG:2154)."),
  list("Nearest-facility distance", "Straight-line distance from each centroid to the nearest facility, overall and per category."),
  list("Facilities within 500 m / 1 km", "Count facilities within fixed radii of each centroid."),
  list("Compare across socioeconomic groups", "Population-weighted summaries by poverty quintile, correlation and exploratory regression.")
)

limitations <- list(
  list("Straight-line distance", "Distances are Euclidean, not walking or public-transport travel times; barriers such as the Seine, rail lines or large parks are ignored."),
  list("Centroid approximation", "Every resident of a cell is placed at its centroid, which adds up to ~140 m of positional uncertainty."),
  list("City-limit boundary effect", "Facilities outside the city limits are excluded, so distances for edge cells (including cells straddling the boundary) may be overstated."),
  list("Availability is not participation", "Proximity to a facility does not measure whether residents use it, can afford it or find it relevant."),
  list("Unequal category sizes", "Categories range from 5 to about 2,000 facilities, so distances are not comparable in absolute terms across categories."),
  list("Area-level indicators", "Socioeconomic variables describe cells, not individuals; associations should not be read at the individual level."),
  list("Cell-based quintiles", "Poverty quintiles split cells, not residents, into equal groups; group populations differ."),
  list("Spatial autocorrelation", "Correlation and regression treat cells as independent. Neighbouring cells are similar, so p-values are too optimistic."),
  list("Approximate social-housing indicator", "Computed as social-housing dwellings divided by households (log_soc / men); it mixes dwelling and household counts and should be interpreted cautiously.")
)

mod_methodology_ui <- function(id) {
  ns <- NS(id)

  numbered_steps <- tags$ol(
    class = "pipeline-steps",
    map(pipeline_steps, \(step) tags$li(strong(step[[1]]), tags$span(step[[2]])))
  )

  limitation_items <- tags$ul(
    class = "limitations",
    map(limitations, \(item) tags$li(strong(paste0(item[[1]], ".")), " ", item[[2]]))
  )

  tagList(
    div(
      class = "page-intro",
      h2("Methodology"),
      p(
        class = "lead",
        "This app is a read-only exploration layer. All distances and counts are precomputed by the",
        "project's R pipeline (R/01–07) and loaded from data/processed/."
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      card(
        card_header(icon("landmark"), "Ministry of Culture — Basilic"),
        p("Base des lieux et équipements culturels: location, type and domain of cultural facilities across France, filtered to Paris.")
      ),
      card(
        card_header(icon("border-all"), "INSEE Filosofi 200 m grid"),
        p("Gridded population and socioeconomic data: residents, households, poor households, owners, single parents, social housing and living standard.")
      ),
      card(
        card_header(icon("map"), "Paris administrative boundaries"),
        p("City and arrondissement limits used to restrict facilities and grid cells to Paris and to label facilities by arrondissement.")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(card_header("Processing pipeline"), numbered_steps),
      card(card_header("Limitations"), limitation_items)
    ),
    card(
      fill = FALSE,
      card_header("How to read the results"),
      p(
        "The analysis describes spatial patterns and associations. Overall cultural access does not show a",
        "simple monotonic poverty gradient, and different categories show different socioeconomic patterns:",
        "some are farther from high-poverty cells, others are closer. Socioeconomic indicators explain only a",
        "small share of the variation in, for example, cinema accessibility. None of these results",
        "establish causal relationships."
      ),
      p(
        class = "mb-0 text-muted small",
        "Key definitions — population-weighted mean: the average distance experienced by a resident,",
        "weighting each cell by its population (ind). Poverty rate: poor households / households (men_pauv / men).",
        "Mean living standard: summed living standard / residents (ind_snv / ind)."
      )
    )
  )
}

mod_methodology_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static page; no server logic required.
  })
}
