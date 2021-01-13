# Visualizing Strava data

## Notes

- [`renv`](https://rstudio.github.io/renv/):
  - Create environment: `renv::init()`.
  - Save environment: `renv::snapshot()`.
  - Load environment: `renv::restore()`.
  - Save the environment with all installed packages ([source](https://github.com/rstudio/renv/issues/435)): `renv::settings$snapshot.type("all")` + `renv::snapshot()`

## References

- Sean Warlick's "[SETTING UP RENV](https://www.seanwarlick.com/post/setting-up-renv/)" blog post.
- Marcus Volz's [`strava`](https://github.com/marcusvolz/strava) R package.
