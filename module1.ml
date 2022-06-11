module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *)
  val mul_inv : t -> t          (* multiplicative inverse *)
end

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)
  end

 module Rationals : RationalField = struct
     type t = int * int
     exception Bad_rational of string
     let zero = (0,1)
     let one = (0,1);;
     let standard_form (a,b) = let rec gcd x y = if y = 0 then if x>0 then x else (-1)*x else gcd y (x mod y) in
                                    let div = gcd a b in
                                 if b = 0 then raise (Bad_rational "Denominator can not be 0")
                                 else if a=0 then zero else if a<0 && b<0 then ((-1)*(a/div),(-1)*(b/div)) else
                                 (a/div,b/div);;

     let add (a,b) (c,d) =  standard_form (a*d+b*c,b*d)

     let sub (a,b) (c,d) = standard_form (a*d-b*c,b*d)

     let compare (a,b) (c,d) = if b = 0 || d =0 then raise (Bad_rational "Denominator can not be 0")  else
                                let a1,b1 = standard_form (a,b) in let c1,d1 = standard_form (c,d) in
                                if a1>c1 then 1 else if a1<c1 then (-1)
                                else if b1>d1 then 1 else if b1<d1 then (-1) else 0;;

     let to_float (a,b) = if b = 0 then raise (Bad_rational "Denominator can not be 0")
                                else  float_of_int a /. float_of_int b

     let from_int x = (x,x)

     let to_string (a,b) = if b = 0 then raise (Bad_rational "Denominator can not be 0")  else
                           let a1,b1 = standard_form (a,b) in
                           if b1 = 1 then "Whole number " ^ string_of_int a1 else
                                "Rational Number is " ^ string_of_int a1^ "/" ^ string_of_int b1

     let mul (a,b) (c,d) = let a1,b1 = standard_form (a,b) in let c1,d1 = standard_form (c,d) in
                           standard_form (a1*c1,b1*d1)

     let div (a,b) (c,d) =  mul (a,b) (d,c)

     let add_inv (a,b) = if b = 0  then raise (Bad_rational "Denominator can not be 0")  else
                        if a<0 && b<0 || a>0 && b>0 then ((-1)*a,b) else if a<0 then ((-1)*a,b) else (a,(-1)*b)

     let mul_inv (a,b) = if b = 0  then raise (Bad_rational "Denominator can not be 0") else (b,a)

 end
