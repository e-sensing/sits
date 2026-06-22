**design patterns:**

- internal functions MUST start with a dot followed by a short prefix. Examples:
  - ".classify_" -> api classify
  - ".raster_" -> api raster
  - ".block_" -> api block
- any raster operation MUST be done using sits raster api; sits raster api is an abstraction layer that uses terra package behind the scenes to run raster operations.
