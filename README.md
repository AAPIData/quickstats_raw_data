# quickstats_raw_data
Quickstats Raw Data - AAPI Data

1. create help functions for each topic: (input includes "year", "geography")
2. create cleanup script that call each topics at one geo level and merge into one df
3. write out csv file that display all topics in one geo level in a long format
4. columns should include: state, county/district, group, topic, topic_type, estimate_type, estimate
5. name each csv file as "state.csv", "county.csv", ect
6. store all csv files under raw_cleanup folder

## Margin of Error Rule

- Use `tidycensus` to pull down margin of error for each count, if the MOE for a count is greater than 25% of the actual estimate, we need to drop it because it is unreliable.
