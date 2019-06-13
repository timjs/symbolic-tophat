module Tax

import TopHat


// Types //

:: Date :== Int
:: Amount :== Int


// Helpers //

// Tasks //

provideCitizenInformation :: Task Date
provideCitizenInformation = enter "Today's date"

provideDocuments :: Task ( Amount, Date )
provideDocuments = enter "Invoice amount" <&> enter "Invoice date"

companyConfirm = ( "Deny", True, done False ) <?> ( "Confirm", True, done True )

officerApprove invoiceDate today confirmed =
  ( "Disapprove", True, done False ) <?> ( "Approve", today - invoiceDate < 365 && confirmed, done True )

main =
  provideCitizenInformation >>= \today ->
  provideDocuments <&> companyConfirm >>= \( ( invoiceAmount, invoiceDate), confirmed ) ->
  officerApprove invoiceDate today confirmed >>= \approved ->
  let subsidyAmount = if approved (min 600 (invoiceAmount / 10)) (0) in
  view "Overview" ( subsidyAmount, approved, confirmed, invoiceDate, today )


// Boilerplate //

Start world = run main world
