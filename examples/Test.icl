module Test

import TopHat


main =
  view "Task" (update "Your age" 30)


// Boilerplate //

Start world = run main world
