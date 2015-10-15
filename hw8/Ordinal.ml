open Cnf ;;
open CalcParser ;;

module Ordinal = struct

	let rec ord2CNF (ord: CalcParser.ordinal) : Cnf.cnf = match ord with
	| Int n -> Cnf.Atom n
	| Omega -> Cnf.List (((Cnf.Atom 1), 1), (Cnf.Atom 0))
	| Add (o1, o2) -> Cnf.add (ord2CNF o1) (ord2CNF o2)
	| Mul (o1, o2) -> Cnf.mul (ord2CNF o1) (ord2CNF o2)
	| Exp (o1, o2) -> Cnf.exp (ord2CNF o1) (ord2CNF o2) ;;

end;;