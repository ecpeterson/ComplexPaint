(***
	paints complex functions into a BMP
 ***)

open Complex

let pi = 2. *. acos(0.)

let period = 2. *. pi
let phaseshift = pi /. 4.

let c_to_hsv value =
	let h = ((Complex.arg value) +. pi) /. (2. *. pi) in
	let s = Pervasives.sqrt ((min (cos (((Complex.norm value) +. period /. 2. +. phaseshift) *.
						 period /. (2. *. pi))) 0.) +. 1.) in
	let v = Pervasives.sqrt ((min (cos ((Complex.norm value) +. phaseshift) *.
						 period /. (2. *. pi)) 0.) +. 1.) in
	(h, s, v)

let hsv_to_rgb (h, s, v) =
	let i = if h = 1. then 0 else int_of_float (h *. 6.) in
	let (fractionalpart, _) = modf (h *. 6.) in
	let p = v *. (1. -. s) in
	let q = v *. (1. -. s *. fractionalpart) in
	let t = v *. (1. -. s *. (1. -. fractionalpart)) in
	match (s, i) with
	| (0., _) -> (v, v, v)
	| (_, 0) -> (v, t, p)
	| (_, 1) -> (q, v, p)
	| (_, 2) -> (p, v, t)
	| (_, 3) -> (p, q, v)
	| (_, 4) -> (t, p, v)
	| _ -> (v, p, q)

let f z =
	let z2 = Complex.mul z z in
	let fact1 = Complex.sub z2 Complex.one in
	let fact2 =
		let temp = Complex.add z {re = 2.; im = 1.} in
		Complex.mul temp temp in
	let fact3 = Complex.add z2 {re = 2.; im = 2.} in
	Complex.div (Complex.mul fact1 fact2) fact3

let g z =
	Complex.exp (Complex.div Complex.one z)

let h z =
	let z2 = Complex.mul z z in
	Complex.div (Complex.sub z2 Complex.one)
				(Complex.add z2 Complex.one)

let run_program f xorigin yorigin width resolution savename =
	let img = Rgb24.create resolution resolution in
	for i = 0 to resolution - 1 do
		for j = 0 to resolution - 1 do
			let ifloat =  width /. float_of_int resolution *.
						  (float_of_int resolution /. 2. -. float_of_int i) -. xorigin in
			let jfloat =  width /. float_of_int resolution *.
						 ((float_of_int resolution /. 2. -. float_of_int j)) -. yorigin in
			let (input_value : Complex.t) = {re = ifloat; im = jfloat} in
			let output_value = f input_value in
			let (r, g, b) = hsv_to_rgb (c_to_hsv output_value) in
			Rgb24.set img i j { Color.r = int_of_float(r *. 255.);
								      g = int_of_float(g *. 255.);
								      b = int_of_float(b *. 255.) }
		done
	done;
	Bmp.save savename [] (Images.Rgb24 img)

let () =
	let resolution = 1000 in
	run_program f 1. 0. 6. resolution "f.bmp"; print_string "f done\n";
	run_program g 0.01 0. 0.75 resolution "g.bmp"; print_string "g done\n";
	run_program h 0.01 0. 3. resolution "h.bmp"; print_string "h done\n";
	()