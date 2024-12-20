# Changing event times

``` mermaid
graph TD
  A["User enters new event time"]-->B["Is there a GPS fix for the event time?"]--> |Yes| C["Assign the GPS fix that matches the event time to the event."]
  B["Is there a GPS fix for the event time?"]--> |No| D["Are there GPS fixes before and after the new event time that are within XX seconds of the event time?"]
  D["Are there GPS fixes before and after the new event time that are within XX seconds of the event time?"]--> |Yes| E["Assign by GPS fix by interpolation."]
  D["Are there GPS fixes before and after the new event time that are within XX seconds of the event time?"]--> |No| F["No automatic GPS fix (leave blank)"]
  F["No automatic GPS fix (leave blank)"]--> G["Are GPS data available from a backup data source?"]
  subgraph User steps
    G["Are GPS data available from a backup data source?"]--> |Yes| H["Import .gps file into Poseidon"]
    G["Are GPS data available from a backup data source?"]--> |No| I["Manually enters coordinates from paper forms."]
  end
  H["Import .gps file into Poseidon"]--> A["User enters new event time"]
```


# Importing .gps data

``` mermaid
graph TD
  A["User imports .gps file"]-->["Are there already position data for the date/time?"]
  subgraph For each fix in the .gps file
    B["Are there already position data for the date/time?"]--> |Yes| C["Do nothing."]
    B["Are there already position data for the date/time?"]--> |No| C["Insert location and date/time from .gps file."]
  end
```