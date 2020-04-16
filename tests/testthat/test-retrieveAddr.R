test_that("Retrieves known address", {
  if (identical(Sys.getenv("arcgisaddr"), "")) {
    skip("No authentication available")
  }
  dt <- data.frame(address="1 Churh Street",address2="Awapuni",address3="",city="Palmersto North",postal="4412",countryCode="NZL")
  dt <- rbind(dt,data.frame(address="2 Churh Street",address2="Awauni",address3="",city="Palmerston North",postal="4402",countryCode="NZL"))
  url <- Sys.getenv("arcgisaddr")
  data <- addressFinder(address_table = dt,url = url)
  known_output <- structure(
    list(
      ResultID = 1:2,
      Loc_name = c("World", "World"),
      Status = c("M", "M"),
      Score = c(91.27, 90.95),
      Match_addr = c(
        "1 Church St, Awapuni, Palmerston North, 4412",
        "2 Church St, Awapuni, Palmerston North, 4412"
      ),
      LongLabel = c(
        "1 Church St, Awapuni, Palmerston North, 4412, NZL",
        "2 Church St, Awapuni, Palmerston North, 4412, NZL"
      ),
      ShortLabel = c("1 Church St",
                     "2 Church St"),
      Addr_type = c("StreetAddressExt", "StreetAddressExt"),
      Type = c("", ""),
      PlaceName = c("", ""),
      Place_addr = c(
        "1 Church St, Palmerston North, 4412",
        "2 Church St, Palmerston North, 4412"
      ),
      Phone = c("", ""),
      URL = c("", ""),
      Rank = c(20L, 20L),
      AddBldg = c("", ""),
      AddNum = c("1", "2"),
      AddNumFrom = c("21", "20"),
      AddNumTo = c("31",
                   "36"),
      AddRange = c("21-31", "20-36"),
      Side = c("L", "R"),
      StPreDir = c("", ""),
      StPreType = c("", ""),
      StName = c("Church",
                 "Church"),
      StType = c("St", "St"),
      StDir = c("", ""),
      BldgType = c("",
                   ""),
      BldgName = c("", ""),
      LevelType = c("", ""),
      LevelName = c("",
                    ""),
      UnitType = c("", ""),
      UnitName = c("", ""),
      SubAddr = c("",
                  ""),
      StAddr = c("1 Church St", "2 Church St"),
      Block = c("",
                ""),
      Sector = c("", ""),
      Nbrhd = c("Awapuni", "Awapuni"),
      District = c("Awapuni", "Awapuni"),
      City = c("Palmerston North",
               "Palmerston North"),
      MetroArea = c("", ""),
      Subregion = c("",
                    ""),
      Region = c("", ""),
      RegionAbbr = c("", ""),
      Territory = c("",
                    ""),
      Zone = c("", ""),
      Postal = c("4412", "4412"),
      PostalExt = c("",
                    ""),
      Country = c("NZL", "NZL"),
      LangCode = c("ENG", "ENG"),
      Distance = c(0L, 0L),
      X = c(175.59225690366, 175.592370822255),
      Y = c(-40.3677231777451,-40.3677069036601),
      DisplayX = c(175.59225690366,
                   175.592370822255),
      DisplayY = c(-40.3677231777451,-40.3677069036601),
      Xmin = c(175.59125690366, 175.591370822255),
      Xmax = c(175.59325690366,
               175.593370822255),
      Ymin = c(-40.3687231777451,-40.3687069036601),
      Ymax = c(-40.3667231777451,-40.3667069036601),
      ExInfo = c("AWAPUNI",
                 "AWAUNI | 4402"),
      address = structure(
        1:2,
        .Label = c("1 Churh Street",
                   "2 Churh Street"),
        class = "factor"
      ),
      address2 = structure(1:2, .Label = c("Awapuni",
                                           "Awauni"), class = "factor"),
      address3 = structure(c(1L,
                             1L), .Label = "", class = "factor"),
      city = structure(
        1:2,
        .Label = c("Palmersto North",
                   "Palmerston North"),
        class = "factor"
      ),
      postal = structure(1:2, .Label = c("4412",
                                         "4402"), class = "factor"),
      countryCode = structure(c(1L,
                                1L), .Label = "NZL", class = "factor")
    ),
    row.names = c(NA,-2L),
    class = "data.frame"
  )
  expect_identical(data$Match_addr,known_output$Match_addr)
})
