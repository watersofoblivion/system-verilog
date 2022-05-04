open Format

type m =
  | One
  | Ten
  | Hundred

type u =
  | Sec
  | Milli
  | Micro
  | Nano
  | Pico
  | Femto

type s = { scale_oom: m; scale_unit: u }
type t = { time_unit: s; time_precision: s }

(* Pretty Printing *)

let pp_m = function
  | One -> dprintf "1"
  | Ten -> dprintf "10"
  | Hundred -> dprintf "100"

let pp_u = function
  | Sec -> dprintf "s"
  | Milli -> dprintf "ms"
  | Micro -> dprintf "us"
  | Nano -> dprintf "ns"
  | Pico -> dprintf "ps"
  | Femto -> dprintf "fs"

let pp_s scale =
  dprintf "%t%t" (pp_m scale.scale_oom) (pp_u scale.scale_unit)

let pp ts =
  dprintf "%t/%t" (pp_s ts.time_unit) (pp_s ts.time_precision)

(* Utilities *)

let m_to_int = function
  | One -> 0
  | Ten -> 1
  | Hundred -> 2

let u_to_int = function
  | Sec -> 5
  | Milli -> 4
  | Micro -> 3
  | Nano -> 2
  | Pico -> 1
  | Femto -> 0

(* Rounding *)

let places ts =
  let scale_to_int s = (u_to_int s.scale_unit) * 3 + (m_to_int s.scale_oom) in
  let yoonit = scale_to_int ts.time_unit in
  let prec = scale_to_int ts.time_precision in
  yoonit - prec

(* Comparison *)

let compare_m m m' = m_to_int m - m_to_int m'
let compare_u u u' = u_to_int u - u_to_int u'

let compare scale scale' =
  let u = compare_u scale.scale_unit scale'.scale_unit in
  if u <> 0
  then u
  else compare_m scale.scale_oom scale'.scale_oom

(* Equality *)

let equal_m m m' = compare_m m m' = 0
let equal_u u u' = compare_u u u' = 0
let equal_s scale scale' = compare scale scale' = 0

let equal ts ts' =
  equal_s ts.time_unit ts'.time_unit
    && equal_s ts.time_precision ts'.time_precision

(* Exceptions *)

exception InvalidTimescale of { time_unit: s; time_precision: s }

(* Constructors *)

let one = One
let ten = Ten
let hundred = Hundred

let s = Sec
let ms = Milli
let us = Micro
let ns = Nano
let ps = Pico
let fs = Femto

let scale oom yoonit = { scale_oom = oom; scale_unit = yoonit }

let create yoonit prec =
  if compare yoonit prec >= 0
  then { time_unit = yoonit; time_precision = prec }
  else
    InvalidTimescale { time_unit = yoonit; time_precision = prec }
      |> raise
