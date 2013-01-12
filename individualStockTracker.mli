open Stock;;

val stockIsPresent : string -> Stock.exchange -> (Stock.stockQuote option * bool)
val insertStockQuote : stockQuote -> unit
val stockIsPresent : string -> exchange -> (stockQuote option*bool)
val insertStockQuoteWithChannels : stockQuote -> in_channel -> out_channel -> unit
val stockIsPresentWithChannels : string -> exchange -> in_channel -> out_channel -> (stockQuote option*bool)