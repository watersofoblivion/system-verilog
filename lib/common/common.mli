(** {1 Common} *)

open Format

module Timescale : sig
  (**
   * {1 Time Scales}
   *
   * Simulation time units and precision as defined in Clause 3.14.
   *)

  (** {2 Data Types} *)

  type m = private
    | One     (** Zero orders of magnitude *)
    | Ten     (** One order of magnitude *)
    | Hundred (** Two orders of magnitude *)
  (** Order of magnitude *)

  type u = private
    | Sec
    | Milli
    | Micro
    | Nano
    | Pico
    | Femto
  (** Time units *)

  type s = private {
    scale_oom:  m; (** The order of magnitude of the scale *)
    scale_unit: u (** The unit of the scale *)
  }
  (** A scale *)

  type t = private {
    time_unit:      s; (** The units of the time scale *)
    time_precision: s  (** The precision of the time scale *)
  }
  (** A timescale *)

  (** {2 Exceptions} *)

  exception InvalidTimescale of {
    time_unit:      s; (** The specified time unit *)
    time_precision: s  (** The specified time precision *)
  }
  (** Raised when an invalid timescale is constructed. *)

  (** {2 Constructors} *)

  (** {3 Orders of Magnitude} *)

  val one : m
  (** [one] creates a zero order-of-magnitude. *)

  val ten : m
  (** [ten] creates one order-of-magnitude. *)

  val hundred : m
  (** [hundred] creates two orders-of-magnitude. *)

  (** {3 Units} *)

  val s : u
  (** [s] creates a unit of seconds. *)

  val ms : u
  (** [ms] creates a unit in milliseconds. *)

  val us : u
  (** [us] creates a unit in microseconds. *)

  val ns : u
  (** [ns] creates a unit in nanoseconds. *)

  val ps : u
  (** [ps] creates a unit in picoseconds. *)

  val fs : u
  (** [fs] creates a unit in femtoseconds. *)

  (** {3 Scales} *)

  val scale : m -> u -> s
  (** [scale oom yoonit] creates a scale of [oom] [yoonit]s. *)

  (** {3 Timescales} *)

  val create : s -> s -> t
  (** [create yoonit prec] creates a timescale with units [yoonit] and precision
      [prec].  Raises {!InvalidTimescale} if [prec] is larger than [yoonit]. *)

  (** {2 Pretty-Printing} *)

  val pp_m : m -> formatter -> unit
  (** [pp_m oom fmt] pretty-prints the order of magnitude [oom] to the
      formatter [fmt]. *)

  val pp_u : u -> formatter -> unit
  (** [pp_u yoonit fmt] pretty-prints the unit [yoonit] to the formatter
      [fmt]. *)

  val pp_s : s -> formatter -> unit
  (** [pp_s scale fmt] pretty-prints the scale [scale] to the formatter
      [fmt]. *)

  val pp : t -> formatter -> unit
  (** [pp ts fmt] pretty-prints the timescale [ts] to the formatter [fmt]. *)

  (** {2 Rounding} *)

  val places : t -> int
  (** [places ts] returns the number of places to round time values to when
      using the time scale [ts]. *)

  (** {2 Comparison} *)

  val compare_m : m -> m -> int
  (** [compare_m m m'] compares the magnitues [m] and [m'].  Returns a negative
      integer if the first is smaller, zero if they are equal, or a positive
      integer is the first is greater. *)

  val compare_u : u -> u -> int
  (** [compare_u u u'] compares the units [u] and [u'].  Returns a negative
      integer if the first is smaller, zero if they are equal, or a positive
      integer is the first is greater. *)

  val compare : s -> s -> int
  (** [compare scale scale'] compares the scales [scale] and [scale'].  Returns
      a negative integer if the first is finer-grained, zero if they are equal,
      or a positive integer if the first is coarser-grained. *)

  (** {2 Equality} *)

  val equal_m : m -> m -> bool
  (** [equal_m m m'] returns [true] if the magnitudes [m] and [m'] are equal, or
      [false] otherwise. *)

  val equal_u : u -> u -> bool
  (** [equal_u u u'] returns [true] if the units [u] and [u'] are equal, or
      [false] otherwise. *)

  val equal_s : s -> s -> bool
  (** [equal_s scale scale'] returns [true] if the scales [scale] and [scale']
      are equal, or [false] otherwise. *)

  val equal : t -> t -> bool
  (** [equal_s ts ts'] returns [true] if the timescales [ts] and [ts'] are
      equal, or [false] otherwise. *)
end
(** Simulation Timescales *)
