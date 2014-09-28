module type Integrable = sig
	(** This is the underlying space. **)
	type t
	(** Multiply a value from the underlying space with a value from the target **)
	val mul : t -> Complex.t -> Complex.t
	(** Functions on the underlying space **)
	type f = t -> Complex.t
	(** Take the integral over the entire space **)
	val integrate : f -> Complex.t
	(** These functions also must be differentiable **)
	val differentiate : f -> f
end;;

module OneDimensional = struct
	type t = int
	type f = t -> Complex.t
	let integrate f =
		List.fold_left Complex.add Complex.zero
			( List.map f [-5;-4;-3;-2;-1;0;1;2;3;4;5])
	let differentiate (func:f):f = fun x ->
		Complex.div (Complex.sub (func (x+1)) (func (x-1)))
			{Complex.re=2.;im=0.}
	let mul x y = Complex.mul {Complex.re = (float_of_int x); im = 0.} y
end;;

let (<|) f g = fun x -> f (g x)

module Make_braket (M : Integrable) = struct
	open Complex
	type ket = M.f

	type bra = M.f -> Complex.t

	type operator = ket -> ket

	let make_bra (k:ket):bra =
		(fun k' -> M.integrate (fun x -> mul (conj (k x)) (k' x)))
	
	let bra_op (b:bra) (o:operator) : bra = b <| o

	let normalize (k:ket) =
		let normalization_factor =
			let current_norm = (make_bra k) k in
			div one (Complex.sqrt current_norm)
		in (fun x -> Complex.mul normalization_factor (k x))
	
	let expectation_value (op:operator) k =
		(make_bra k) @@ op k
	
	let position_operator:operator =
		fun k -> fun x -> M.mul x (k x)
	let momentum_operator:operator =
		fun k -> fun x -> Complex.mul i (M.differentiate k @@ x)
end;;

module Simple = Make_braket(OneDimensional)
let psi x = {Complex.re = (float_of_int (5 - x)) ; im = 0.} 
let psi' = Simple.make_bra(psi)
