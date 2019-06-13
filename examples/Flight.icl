module Flight

import TopHat


// Types //

:: Seat :== Int


// Helpers //

maxSeats :: Seat
maxSeats = 50

bookedSeats :: Ref (List Seat)
bookedSeats = ref "list of booked seats" []


// Tasks //

bookSeat :: Task Unit
bookSeat =
  enter "Seat number" <&> Internal @ watch "Already booked seats" bookedSeats >?>
    [ ( "Continue", \( x, ss ) -> not (elem x ss) && 0 < x && x <= maxSeats, \(x, ss) ->
        bookedSeats <<- cons x )
    ]

main =
  bookSeat <&> bookSeat <&> bookSeat >>= \_ ->
  watch "Already booked seats" bookedSeats


// Boilerplate //

Start world = run main world
