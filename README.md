# flight-tracker

The high-level goal of this project is to find flights based on a user-specified
arrival or destination airport. We will also allow the user to select one of
these flights, and then display more detailed information, such as whether the
flight is delayed. After accomplishing these tasks, we hope to display a map of
the US and show the flight's progress on this map.

Specifically, we will define success for our project by implementing the
following features:
- an information pane that allows you to browse flights between two specific
  airports
- another information pane which includes specific details about a particular
  flight which includes
  - arrival and departure time and location
  - scheduled and estimated departure times
  - plane information (tail tag and call sign)
- for in-progress flights, we should also implement a feature which displays a
  map of the arrival and departure locations along with the current location of
  the plane

We plan to use the [brick library](https://github.com/jtdaugherty/brick/) to 
display the flight information and map to the user. We plan to use the
[req library](https://hackage.haskell.org/package/req) and
[OpenSky API](https://opensky-network.org/apidoc/index.html) to fetch actual
flight data.