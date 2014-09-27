module type Integrable = sig
	(** This is the underlying space. **)
	type t
	(** Functions on the underlying space **)
	type f = t -> Complex.t
	(** Take the integral over the entire space **)
	val integrate : f -> Complex.t
end;;

module OneDimensional = struct
	type t = int
	type f = t -> Complex.t
	let integrate f =
		List.fold_left Complex.add Complex.zero
			( List.map f [-5;-4;-3;-2;-1;0;1;2;3;4;5])
end;;

let (<|) f g = fun x -> f (g x)

module Make_braket (M : Integrable) = struct
	type ket = M.f

	type bra = M.f -> Complex.t

	type operator = ket -> ket

	let make_bra (k:ket):bra =
		(fun k' -> M.integrate (fun x -> Complex.mul (Complex.conj (k x)) (k' x)))
	
	let bra_op (b:bra) (o:operator) : bra = b <| o
	let (!<) (b:bra) : bra = b
	let ($|) (b:bra) (o:operator) = bra_op b o
	let (|$) (b:bra) (k:ket) = b k
	let ($>) (v:Complex.t) _ = v
end;;
