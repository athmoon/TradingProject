val getquotejson : string -> Yojson.Safe.json option

type exchange = NASDAQ | NYSE | CME | INDEXSP | OTHER

type stock = {
	ticker : string;
	exchange : exchange;
}

type stockQuote = {
	stock : stock;
	price : float;
	change : float;
	change_percent : float;
	last_trade_time : string;
	last_trade_time_object: Netdate.t;
	dividend : float option;
	yield : float option;
}

val printStock : stock -> unit
val getStockQuote : string -> stockQuote option
val printStockQuoteOption : stockQuote option -> unit
val parseDateString : string -> Netdate.t

