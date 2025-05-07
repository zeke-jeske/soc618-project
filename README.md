# Zeke's SOC618 (Social Network Analysis) Project

## Data sources

**[LODES Documentation](https://lehd.ces.census.gov/doc/help/onthemap/LODESTechDoc.pdf)**

- 3 data files. OD is origin-destination data, which is the one we need
- File name: `[ST]_od_[PART]_[TYPE]_[YEAR].csv.gz`, where
  - `[ST]`: the state code (e.g. `il` for Illinois)
  - `[PART]`: `main` for residence and workplace in the state, or `aux` for
    workplace in the state but residence outside the state
  - `[TYPE]`: job type. `JT00` for All Jobs
  - `[YEAR]`: 4-digit year. 2000-2022.
- Columns in the data:
  - `w_geocode`: workplace census block code. This is a 15-digit FIPS code. The
    first 11 digits represent the tract.
  - `h_geocode`: residence census block code
  - `S000`: number of jobs
  - `SXXX`: other related variables that could be useful later on
