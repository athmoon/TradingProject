

let getTickers (ex:string) : string list = 
	let rawcsv = Csv.load ex in
	let tickers = Csv.sub 1 0 ((Csv.lines rawcsv) - 1) 1 rawcsv in
	let tempfun s = 
		String.concat "" (Array.to_list s)
		in
	List.map tempfun (Array.to_list (Csv.to_array tickers))

let getNYSETickers () : string list = 
	getTickers "nyse.csv"
	
let getNASDAQTickers () : string list =
	getTickers "nasdaq.csv"