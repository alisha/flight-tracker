# flight-tracker

## Milestone 1: Proposal

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

## Milestone 2: Updates

The architecture of our app is as follows. The core application logic is in the
`src` folder. The code that renders the `brick` interface is in `UI.hs`, and
the code that makes API requests is defined in `Requests.hs`.

So far, we experienced some challenges with parsing the API responses, but we
were able to solve this by using the Aeson library. We now have an application
that takes in user input (an arrival airport, start timestamp, and end
timestamp), and then print the list of flights matching the user-defined
information to the console.

By the deadline, we expect to have an interface that can display this API
output on screen and let users get more detailed information about specific
flights from these search results. Creating the map that tracks a flight's
current progress will be a stretch goal, because the API we are using does
not provide this information in real-time.
