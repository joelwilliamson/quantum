(** Modules satisfy this constraint if the type can be multiplied by and added
    to complex numbers. **)
module type CompatibleWithComplex = sig
	type t
	val mul : t -> Complex.t -> Complex.t
	val add : t -> Complex.t -> Complex.t
end

(** Modules satisfy this constraint if they define a function that is integrable **)
module type Integrable = sig
	type t
	type f = t -> Complex.t
	val integrate : f -> Complex.t
end

(** Modules satisfy this constraint if they can be differentiated **)
module type Differentiable = sig
	type t
	type f = t -> Complex.t
	val differentiate : f -> f
end

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
	let add i c = Complex.add {Complex.re = (float_of_int i); im = 0.} c
end

let (<|) f g = fun x -> f (g x)

module Make_braket (A : CompatibleWithComplex)
		(I : Integrable with type t=A.t)
		(D : Differentiable with type t = A.t) = struct
	open Complex
	type ket = I.f

	type bra = I.f -> Complex.t

	type operator = ket -> ket

	let make_bra (k:ket):bra =
		(fun k' -> I.integrate (fun x -> mul (conj (k x)) (k' x)))
	
	let bra_op (b:bra) (o:operator) : bra = b <| o

	let normalize (k:ket) =
		let normalization_factor =
			let current_norm = (make_bra k) k in
			div one (Complex.sqrt current_norm)
		in (fun x -> Complex.mul normalization_factor (k x))
	
	let expectation_value (op:operator) k =
		(make_bra k) @@ op k
	
	let position_operator:operator =
		fun k -> fun x -> A.mul x (k x)
	let momentum_operator:operator =
		fun k -> fun x -> Complex.mul i (D.differentiate k @@ x)
	
	let squared (op:operator) : operator = op <| op
end

module Simple = Make_braket(OneDimensional)(OneDimensional)(OneDimensional)
let psi x = {Complex.re = (float_of_int (5 - (abs x))) ; im = 0.}
let psi' = Simple.make_bra(psi)
