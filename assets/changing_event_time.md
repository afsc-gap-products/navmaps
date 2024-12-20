# Changing event times

``` mermaid
graph TD
  A["User enters new event time"]-->B["Is there a GPS fix for<br>the event time?"]--> |Yes| C["Assign the GPS fix that matches<br>the event time to the event."]
  B["Is there a GPS fix for<br>the event time?"]--> |No| D["Are there GPS fixes before and<br>after the new event time that<br>are within XX seconds of the<br>event time?"]
  D--> |Yes| E["Assign by GPS fix by<br>interpolating<br>between fixes."]
  D--> |No| F["No automatic GPS fix (leave blank)"]
  subgraph User steps
    F--> G["Are GPS data<br>available from a<br>backup data source?"]
    G--> |Yes| H["Import .gps file into<br>Poseidon"]
    G--> |No| I["Manually enters coordinates<br>from paper forms."]
```
