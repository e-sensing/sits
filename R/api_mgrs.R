# The following code is a translation of parts of the proj4js/mgrs repository
# (https://github.com/proj4js/mgrs) which is licensed under the MIT License.
#
# Copyright (c) 2012, Mike Adair, Richard Greenwood, Didier Richard,
# Stephen Irons, Olivier Terral, Calvin Metcalf
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


# UTM zones are grouped, and assigned to one of a group of 6 sets.
NUM_100K_SETS <- 6

# The column letters (for easting) of the lower left value, per set.
SET_ORIGIN_COLUMN_LETTERS <- 'AJSAJS'

# The row letters (for northing) of the lower left value, per set.
SET_ORIGIN_ROW_LETTERS <- 'AFAFAF'

A <- 65  # A
I <- 73  # I
O <- 79  # O
V <- 86  # V
Z <- 90  # Z

# First eccentricity squared
ECC_SQUARED <- 0.00669438

# Scale factor along the central meridian
SCALE_FACTOR <- 0.9996

# Semimajor axis (half the width of the earth) in meters
SEMI_MAJOR_AXIS <- 6378137

# The easting of the central meridian of each UTM zone
EASTING_OFFSET <- 500000

# The northing of the equator for southern hemisphere locations (in UTM)
NORTHING_OFFSET <- 10000000

# UTM zone width in degrees
UTM_ZONE_WIDTH <- 6

# Half the width of a UTM zone in degrees
HALF_UTM_ZONE_WIDTH <- UTM_ZONE_WIDTH / 2

# Convert lat/lon to MGRS.
#
# ll: Array with longitude and latitude on a WGS84 ellipsoid.
# accuracy: Accuracy in digits (5 for 1 m, 4 for 10 m, 3 for 100 m, 2 for 1 km, 1 for 10 km or 0 for 100 km).
#   Optional, default is 5.
# Returns: The MGRS string for the given location and accuracy.
forward <- function(ll, accuracy = 5) {
    if (!is.numeric(accuracy)) {
        accuracy <- 5  # default accuracy 1m
    }

    if (!is.vector(ll) || length(ll) != 2 || any(sapply(ll, function(x) !is.numeric(x)))) {
        stop("forward did not receive a numeric array")
    }

    lon <- ll[1]
    lat <- ll[2]

    if (lon < -180 || lon > 180) {
        stop(paste("forward received an invalid longitude of", lon))
    }

    if (lat < -90 || lat > 90) {
        stop(paste("forward received an invalid latitude of", lat))
    }

    if (lat < -80 || lat > 84) {
        stop("forward received a latitude of", lat, "but this library does not support conversions of points in polar regions below 80°S and above 84°N")
    }

    utm <- LLtoUTM(lat = lat, lon = lon)
    return(encode(utm, accuracy))
}

# Convert MGRS to lat/lon bounding box.
#
# mgrs: MGRS string.
# Returns: An array with left (longitude), bottom (latitude), right
#   (longitude), and top (latitude) values in WGS84, representing the
#   bounding box for the provided MGRS reference.
inverse <- function(mgrs) {
    bbox <- UTMtoLL(decode(toupper(mgrs)))
    if (!is.null(bbox$lat) && !is.null(bbox$lon)) {
        return(c(bbox$lon, bbox$lat, bbox$lon, bbox$lat))
    }
    return(c(bbox$left, bbox$bottom, bbox$right, bbox$top))
}

toPoint <- function(mgrs) {
    if (mgrs == '') {
        stop("toPoint received a blank string")
    }
    bbox <- UTMtoLL(decode(toupper(mgrs)))
    if (!is.null(bbox$lat) && !is.null(bbox$lon)) {
        return(c(bbox$lon, bbox$lat))
    }
    return(c((bbox$left + bbox$right) / 2, (bbox$top + bbox$bottom) / 2))
}

# Conversion from degrees to radians.
#
# deg: The angle in degrees.
# Returns: The angle in radians.
degToRad <- function(deg) {
    return(deg * (pi / 180))
}

# Conversion from radians to degrees.
#
# rad: The angle in radians.
# Returns: The angle in degrees.
radToDeg <- function(rad) {
    return(180 * (rad / pi))
}

# Converts a set of Longitude and Latitude co-ordinates to UTM
# using the WGS84 ellipsoid.
#
# ll: Object literal with lat and lon properties representing the WGS84 coordinate to be converted.
# Returns: Object literal containing the UTM value with easting, northing, zoneNumber, and zoneLetter properties,
#   and an optional accuracy property in digits. Returns NULL if the conversion failed.
LLtoUTM <- function(ll) {
    Lat <- ll$lat
    Long <- ll$lon
    a <- SEMI_MAJOR_AXIS
    LatRad <- degToRad(Lat)
    LongRad <- degToRad(Long)
    ZoneNumber <- NULL

    # (int)
    ZoneNumber <- floor((Long + 180) / 6) + 1

    # Make sure the longitude 180 is in Zone 60
    if (Long == 180) {
        ZoneNumber <- 60
    }

    # Special zone for Norway
    if (Lat >= 56 && Lat < 64 && Long >= 3 && Long < 12) {
        ZoneNumber <- 32
    }

    # Special zones for Svalbard
    if (Lat >= 72 && Lat < 84) {
        if (Long >= 0 && Long < 9) {
            ZoneNumber <- 31
        } else if (Long >= 9 && Long < 21) {
            ZoneNumber <- 33
        } else if (Long >= 21 && Long < 33) {
            ZoneNumber <- 35
        } else if (Long >= 33 && Long < 42) {
            ZoneNumber <- 37
        }
    }

    # +HALF_UTM_ZONE_WIDTH puts origin in middle of zone
    LongOrigin <- (ZoneNumber - 1) * UTM_ZONE_WIDTH - 180 + HALF_UTM_ZONE_WIDTH

    LongOriginRad <- degToRad(LongOrigin)

    eccPrimeSquared <- ECC_SQUARED / (1 - ECC_SQUARED)

    N <- a / sqrt(1 - ECC_SQUARED * sin(LatRad) * sin(LatRad))
    T <- tan(LatRad) * tan(LatRad)
    C <- eccPrimeSquared * cos(LatRad) * cos(LatRad)
    A <- cos(LatRad) * (LongRad - LongOriginRad)

    M <- a * ((1 - ECC_SQUARED / 4 - 3 * ECC_SQUARED * ECC_SQUARED / 64 - 5 * ECC_SQUARED * ECC_SQUARED * ECC_SQUARED / 256) * LatRad -
                  (3 * ECC_SQUARED / 8 + 3 * ECC_SQUARED * ECC_SQUARED / 32 + 45 * ECC_SQUARED * ECC_SQUARED * ECC_SQUARED / 1024) * sin(2 * LatRad) +
                  (15 * ECC_SQUARED * ECC_SQUARED / 256 + 45 * ECC_SQUARED * ECC_SQUARED * ECC_SQUARED / 1024) * sin(4 * LatRad) -
                  (35 * ECC_SQUARED * ECC_SQUARED * ECC_SQUARED / 3072) * sin(6 * LatRad))

    UTMEasting <- (SCALE_FACTOR * N * (A + (1 - T + C) * A * A * A / 6 +
                                           (5 - 18 * T + T * T + 72 * C - 58 * eccPrimeSquared) * A * A * A * A * A / 120) + EASTING_OFFSET)

    UTMNorthing <- (SCALE_FACTOR * (M + N * tan(LatRad) * (A * A / 2 +
                                                               (5 - T + 9 * C + 4 * C * C) * A * A * A * A / 24 +
                                                               (61 - 58 * T + T * T + 600 * C - 330 * eccPrimeSquared) * A * A * A * A * A * A / 720)))
    if (Lat < 0) {
        UTMNorthing <- UTMNorthing + NORTHING_OFFSET
    }

    return(list(
        northing = trunc(UTMNorthing),
        easting = trunc(UTMEasting),
        zoneNumber = ZoneNumber,
        zoneLetter = getLetterDesignator(Lat)
    ))
}

# Converts UTM coords to lat/long, using the WGS84 ellipsoid. This is a convenience
# class where the Zone can be specified as a single string eg."60N" which
# is then broken down into the ZoneNumber and ZoneLetter.
#
# utm: An object literal with northing, easting, zoneNumber, and zoneLetter properties.
#   If an optional accuracy property is provided (in meters), a bounding box will be returned instead of
#   latitude and longitude.
# Returns: An object literal containing either lat and lon values (if no accuracy was provided),
#   or top, right, bottom, and left values for the bounding box calculated according to the provided accuracy.
#   Returns NULL if the conversion failed.
UTMtoLL <- function(utm) {
    UTMNorthing <- utm$northing
    UTMEasting <- utm$easting
    zoneLetter <- utm$zoneLetter
    zoneNumber <- utm$zoneNumber

    # check the ZoneNumber is valid
    if (zoneNumber < 0 || zoneNumber > 60) {
        return(NULL)
    }

    a <- SEMI_MAJOR_AXIS
    e1 <- (1 - sqrt(1 - ECC_SQUARED)) / (1 + sqrt(1 - ECC_SQUARED))

    # remove 500,000 meter offset for longitude
    x <- UTMEasting - EASTING_OFFSET
    y <- UTMNorthing

    # We must know somehow if we are in the Northern or Southern hemisphere, this is the only time we use the letter
    # So even if the Zone letter isn't exactly correct it should indicate the hemisphere correctly
    if (zoneLetter < "N") {
        y <- y - NORTHING_OFFSET  # remove offset used for southern hemisphere
    }

    # +HALF_UTM_ZONE_WIDTH puts origin in middle of zone
    LongOrigin <- (zoneNumber - 1) * UTM_ZONE_WIDTH - 180 + HALF_UTM_ZONE_WIDTH

    eccPrimeSquared <- ECC_SQUARED / (1 - ECC_SQUARED)

    M <- y / SCALE_FACTOR
    mu <- M / (a * (1 - ECC_SQUARED / 4 - 3 * ECC_SQUARED * ECC_SQUARED / 64 - 5 * ECC_SQUARED * ECC_SQUARED * ECC_SQUARED / 256))

    phi1Rad <- mu + (3 * e1 / 2 - 27 * e1 * e1 * e1 / 32) * sin(2 * mu) + (21 * e1 * e1 / 16 - 55 * e1 * e1 * e1 * e1 / 32) * sin(4 * mu) + (151 * e1 * e1 * e1 / 96) * sin(6 * mu)

    N1 <- a / sqrt(1 - ECC_SQUARED * sin(phi1Rad) * sin(phi1Rad))
    T1 <- tan(phi1Rad) * tan(phi1Rad)
    C1 <- eccPrimeSquared * cos(phi1Rad) * cos(phi1Rad)
    R1 <- a * (1 - ECC_SQUARED) / ((1 - ECC_SQUARED * sin(phi1Rad) * sin(phi1Rad))^(3/2))
    D <- x / (N1 * SCALE_FACTOR)

    lat <- phi1Rad - (N1 * tan(phi1Rad) / R1) * (D * D / 2 - (5 + 3 * T1 + 10 * C1 - 4 * C1 * C1 - 9 * eccPrimeSquared) * D * D * D * D / 24 + (61 + 90 * T1 + 298 * C1 + 45 * T1 * T1 - 252 * eccPrimeSquared - 3 * C1 * C1) * D * D * D * D * D * D / 720)
    lat <- radToDeg(lat)

    lon <- (D - (1 + 2 * T1 + C1) * D * D * D / 6 + (5 - 2 * C1 + 28 * T1 - 3 * C1 * C1 + 8 * eccPrimeSquared + 24 * T1 * T1) * D * D * D * D * D / 120) / cos(phi1Rad)
    lon <- LongOrigin + radToDeg(lon)

    result <- NULL

    if (!is.null(utm$accuracy)) {
        topRight <- UTMtoLL(list(
            northing = utm$northing + utm$accuracy,
            easting = utm$easting + utm$accuracy,
            zoneLetter = utm$zoneLetter,
            zoneNumber = utm$zoneNumber
        ))
        result <- list(
            top = topRight$lat,
            right = topRight$lon,
            bottom = lat,
            left = lon
        )
    } else {
        result <- list(
            lat = lat,
            lon = lon
        )
    }

    return(result)
}

# Calculates the MGRS letter designator for the given latitude.
#
# latitude: The latitude in WGS84 to get the letter designator for.
# Returns: The letter designator.
getLetterDesignator <- function(latitude) {
    if (latitude <= 84 && latitude >= 72) {
        # the X band is 12 degrees high
        return("X")
    } else if (latitude < 72 && latitude >= -80) {
        # Latitude bands are lettered C through X, excluding I and O
        bandLetters <- "CDEFGHJKLMNPQRSTUVWX"
        bandHeight <- 8
        minLatitude <- -80
        index <- floor((latitude - minLatitude) / bandHeight)
        return(substr(bandLetters, index+1, index+1))
    } else if (latitude > 84 || latitude < -80) {
        # This is here as an error flag to show that the Latitude is outside MGRS limits
        return("Z")
    }
}

# Encodes a UTM location as MGRS string.
#
# utm: An object literal with easting, northing, zoneLetter, zoneNumber.
# accuracy: Accuracy in digits (0-5).
# Returns: MGRS string for the given UTM location.
encode <- function(utm, accuracy) {
    # prepend with leading zeroes
    seasting <- paste0("00000", utm$easting)
    snorthing <- paste0("00000", utm$northing)

    return(paste0(utm$zoneNumber, utm$zoneLetter, get100kID(utm$easting, utm$northing, utm$zoneNumber),
                  substr(seasting, nchar(seasting) - 4 + 1, nchar(seasting) - 4 + accuracy),
                  substr(snorthing, nchar(snorthing) - 4 + 1, nchar(snorthing) - 4 + accuracy)))
}

# Get the two-letter 100k designator for a given UTM easting,
# northing, and zone number value.
#
# easting: UTM easting value.
# northing: UTM northing value.
# zoneNumber: UTM zone number.
# Returns: The two-letter 100k designator for the given UTM location.
get100kID <- function(easting, northing, zoneNumber) {
    setParm <- get100kSetForZone(zoneNumber)
    setColumn <- floor(easting / 100000)
    setRow <- floor(northing / 100000) %% 20
    return(getLetter100kID(setColumn, setRow, setParm))
}

# Given a UTM zone number, figure out the MGRS 100K set it is in.
#
# i: UTM zone number.
# Returns: The 100k set the UTM zone is in.
get100kSetForZone <- function(i) {
    setParm <- i %% NUM_100K_SETS
    if (setParm == 0) {
        setParm <- NUM_100K_SETS
    }
    return(setParm)
}

# Get the two-letter MGRS 100k designator given information
# translated from the UTM northing, easting, and zone number.
#
# column: The column index as it relates to the MGRS 100k set spreadsheet,
#         created from the UTM easting. Values are 1-8.
# row: The row index as it relates to the MGRS 100k set spreadsheet,
#      created from the UTM northing value. Values are from 0-19.
# parm: The set block as it relates to the MGRS 100k set spreadsheet,
#       created from the UTM zone. Values are from 1-60.
# Returns: The two-letter MGRS 100k code.
getLetter100kID <- function(column, row, parm) {
    # colOrigin and rowOrigin are the letters at the origin of the set
    index <- parm - 1
    colOrigin <- charToRaw(SET_ORIGIN_COLUMN_LETTERS)[index]
    rowOrigin <- charToRaw(SET_ORIGIN_ROW_LETTERS)[index]

    # colInt and rowInt are the letters to build to return
    colInt <- colOrigin + column - 1
    rowInt <- rowOrigin + row
    rollover <- FALSE

    if (colInt > 90) {
        colInt <- colInt - 90 + 65 - 1
        rollover <- TRUE
    }

    if (colInt == 73 || (colOrigin < 73 && colInt > 73) || ((colInt > 73 || colOrigin < 73) && rollover)) {
        colInt <- colInt + 1
    }

    if (colInt == 79 || (colOrigin < 79 && colInt > 79) || ((colInt > 79 || colOrigin < 79) && rollover)) {
        colInt <- colInt + 1

        if (colInt == 73) {
            colInt <- colInt + 1
        }
    }

    if (colInt > 90) {
        colInt <- colInt - 90 + 65 - 1
    }

    if (rowInt > 86) {
        rowInt <- rowInt - 86 + 65 - 1
        rollover <- TRUE
    } else {
        rollover <- FALSE
    }

    if (((rowInt == 73) || ((rowOrigin < 73) && (rowInt > 73))) || (((rowInt > 73) || (rowOrigin < 73)) && rollover)) {
        rowInt <- rowInt + 1
    }

    if (((rowInt == 79) || ((rowOrigin < 79) && (rowInt > 79))) || (((rowInt > 79) || (rowOrigin < 79)) && rollover)) {
        rowInt <- rowInt + 1

        if (rowInt == 73) {
            rowInt <- rowInt + 1
        }
    }

    if (rowInt > 86) {
        rowInt <- rowInt - 86 + 65 - 1
    }

    twoLetter <- rawToChar(c(colInt, rowInt))
    return(twoLetter)
}

# Decode the UTM parameters from an MGRS string.
#
# mgrsString: An UPPERCASE coordinate string is expected.
# Returns: An object literal with easting, northing, zoneLetter,
#          zoneNumber, and accuracy (in meters) properties.
decode <- function(mgrsString) {
    if (is.null(mgrsString) || nchar(mgrsString) == 0) {
        stop("MGRSPoint converting from nothing", call. = FALSE)
    }

    # Remove any spaces in MGRS String
    mgrsString <- gsub(" ", "", mgrsString)

    length <- nchar(mgrsString)

    hunK <- NULL
    sb <- ""
    i <- 1

    # Get Zone number
    while (!grepl("[A-Z]", testChar <- substr(mgrsString, i, i))) {
        if (i > 3) {
            stop(paste("MGRSPoint bad conversion from:", mgrsString), call. = FALSE)
        }
        sb <- paste(sb, testChar, sep = "")
        i <- i + 1
    }

    zoneNumber <- as.integer(sb)

    if (i == 1 || i + 2 > length) {
        # A good MGRS string has to be 4-5 digits long,
        # ##AAA/#AAA at least.
        stop(paste("MGRSPoint bad conversion from", mgrsString), call. = FALSE)
    }

    zoneLetter <- substr(mgrsString, i, i)
    i <- i + 1

    # Should we check the zone letter here? Why not.
    if (zoneLetter <= "A" || zoneLetter == "B" || zoneLetter == "Y" || zoneLetter >= "Z" || zoneLetter == "I" || zoneLetter == "O") {
        stop(paste("MGRSPoint zone letter", zoneLetter, "not handled:", mgrsString), call. = FALSE)
    }

    hunK <- substr(mgrsString, i, i + 1)

    set <- get100kSetForZone(zoneNumber)

    east100k <- getEastingFromChar(substr(hunK, 1, 1), set)
    north100k <- getNorthingFromChar(substr(hunK, 2, 2), set)

    # We have a bug where the northing may be 2000000 too low.
    # How do we know when to roll over?
    while (north100k < getMinNorthing(zoneLetter)) {
        north100k <- north100k + 2000000
    }

    # Calculate the char index for easting/northing separator
    remainder <- length - i - 1

    if (remainder %% 2 != 0) {
        stop(paste("MGRSPoint has to have an even number of digits after the zone letter and two 100km letters - front half for easting meters, second half for northing meters", mgrsString), call. = FALSE)
    }

    sep <- remainder / 2

    sepEasting <- 0
    sepNorthing <- 0
    accuracyBonus <- 100000 / 10^sep
    if (sep > 0) {
        sepEastingString <- substr(mgrsString, i, i + sep - 1)
        sepEasting <- as.numeric(sepEastingString) * accuracyBonus
        sepNorthingString <- substr(mgrsString, i + sep, length)
        sepNorthing <- as.numeric(sepNorthingString) * accuracyBonus
    }

    easting <- sepEasting + east100k
    northing <- sepNorthing + north100k

    return(list(
        easting = easting,
        northing = northing,
        zoneLetter = zoneLetter,
        zoneNumber = zoneNumber,
        accuracy = accuracyBonus
    ))
}

# Given the first letter from a two-letter MGRS 100k zone, and given the
# MGRS table set for the zone number, figure out the easting value that
# should be added to the other, secondary easting value.
#
# e: The first letter from a two-letter MGRS 100k zone.
# set: The MGRS table set for the zone number.
# Returns: The easting value for the given letter and set.
getEastingFromChar <- function(e, set) {
    # colOrigin is the letter at the origin of the set for the column
    curCol <- utf8ToInt(SET_ORIGIN_COLUMN_LETTERS)[set]
    eastingValue <- 100000
    rewindMarker <- FALSE

    while (curCol != utf8ToInt(e)) {
        curCol <- curCol + 1
        if (curCol == utf8ToInt("I")) {
            curCol <- curCol + 1
        }
        if (curCol == utf8ToInt("O")) {
            curCol <- curCol + 1
        }
        if (curCol > utf8ToInt("Z")) {
            if (rewindMarker) {
                stop(paste("Bad character:", e), call. = FALSE)
            }
            curCol <- utf8ToInt("A")
            rewindMarker <- TRUE
        }
        eastingValue <- eastingValue + 100000
    }

    return(eastingValue)
}

# Given the second letter from a two-letter MGRS 100k zone, and given the
# MGRS table set for the zone number, figure out the northing value that
# should be added to the other, secondary northing value. You have to
# remember that Northings are determined from the equator, and the vertical
# cycle of letters mean a 2000000 additional northing meters. This happens
# approx. every 18 degrees of latitude. This method does *NOT* count any
# additional northings. You have to figure out how many 2000000 meters need
# to be added for the zone letter of the MGRS coordinate.
#
# n: Second letter of the MGRS 100k zone
# set: The MGRS table set number, which is dependent on the UTM zone number.
# Returns: The northing value for the given letter and set.
getNorthingFromChar <- function(n, set) {
    if (n > "V") {
        stop(paste("MGRSPoint given invalid Northing", n), call. = FALSE)
    }

    # rowOrigin is the letter at the origin of the set for the column
    curRow <- utf8ToInt(SET_ORIGIN_ROW_LETTERS)[set]
    northingValue <- 0
    rewindMarker <- FALSE

    while (curRow != utf8ToInt(n)) {
        curRow <- curRow + 1
        if (curRow == utf8ToInt("I")) {
            curRow <- curRow + 1
        }
        if (curRow == utf8ToInt("O")) {
            curRow <- curRow + 1
        }
        # fixing a bug making whole application hang in this loop
        # when 'n' is a wrong character
        if (curRow > utf8ToInt("V")) {
            if (rewindMarker) { # making sure that this loop ends
                stop(paste("Bad character:", n), call. = FALSE)
            }
            curRow <- utf8ToInt("A")
            rewindMarker <- TRUE
        }
        northingValue <- northingValue + 100000
    }

    return(northingValue)
}

# The function getMinNorthing returns the minimum northing value of a MGRS
# zone.
#
# zoneLetter: The MGRS zone to get the min northing for.
# Returns: The minimum northing value for the given zone letter.
getMinNorthing <- function(zoneLetter) {
    northing <- switch(
        zoneLetter,
        "C" = 1100000,
        "D" = 2000000,
        "E" = 2800000,
        "F" = 3700000,
        "G" = 4600000,
        "H" = 5500000,
        "J" = 6400000,
        "K" = 7300000,
        "L" = 8200000,
        "M" = 9100000,
        "N" = 0,
        "P" = 800000,
        "Q" = 1700000,
        "R" = 2600000,
        "S" = 3500000,
        "T" = 4400000,
        "U" = 5300000,
        "V" = 6200000,
        "W" = 7000000,
        "X" = 7900000,
        -1
    )

    if (northing >= 0) {
        return(northing)
    } else {
        stop(paste("Invalid zone letter:", zoneLetter), call. = FALSE)
    }
}
