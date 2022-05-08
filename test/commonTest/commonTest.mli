(** {1 Common Tests} *)

open Format

open OUnit2

open Common

(** {2 Test Suite} *)

val suite : test
(** [suite] is the unit test suite. *)

(** {2 Assertions} *)

val assert_pp : ctxt:test_ctxt -> ('a -> formatter -> unit) -> string list -> 'a -> unit
(** [assert_pp ~ctxt pp lines x] asserts that the pretty-printer [pp] produces
    [lines] (concatenated with newlines) as output when applied to the value
    [x]. *)

val not_equal : ctxt:test_ctxt -> string -> (formatter -> 'a -> unit) -> 'a -> 'a -> unit
(** [not_equal ~ctxt msg pp expected actual] fails a test because [expected] was
    not equal to [actual].  [msg] is the type of objects that are not equal and
    should be a plural form (i.e., "Expressions").  [pp] is used to pretty-print
    the two values. *)

val assert_optional_equal : ctxt:test_ctxt -> string -> (ctxt:test_ctxt -> 'a -> 'a -> unit) -> 'a option -> 'a option -> unit
(** [assert_optional_equal ~ctxt id assert_equal expected actual] asserts that
    the values [expected] and [actual] are either both [Some] and are equal
    using [assert_equal], or both [None].  If they are mismatched, an error
    is created using [id] and passed to {!OUnit2.assert_failure}. *)

module LocTest : sig
  (**
   * {1 Location Tracking Tests}
   *)

  (**
   * {2 Utilities}
   *)

  val dummy : Loc.t
  (** [dummy] is a location guaranteed not to be in any file. *)

  val gen : unit -> Loc.t
  (** [gen ()] generates a new, unique location. *)

  val make : (int * int * int) -> (int * int * int) -> Loc.t
  (** [make (start_line, start_col, start_off) (end_line, end_col, end_off)]
      constructs a location starting at the position
      [(start_line, start_col, start_off)] and ending at the position
      [(end_line, end_col, end_off)]. *)

  val span : Loc.t -> Loc.t -> Loc.t
  (** [span src dst] constructs a location starting at the start position of [src]
      and ending at the end position of [dst]. *)

  val span_from : (int * int * int) -> Loc.t -> Loc.t
  (** [span_from src dst] constructs a location starting at the start position
      [src] and ending at the end position of [dst]. *)

  val shift : (int * int * int) -> Loc.t -> Loc.t
  (** [shift (lines, cols, off) loc] shifts the location [loc] by [lines] lines,
      [cols] columns, and [off] bytes. *)

  (**
   * {2 Assertions}
   *)

  val assert_loc_equal : ctxt:test_ctxt -> Loc.t -> Loc.t -> unit
  (** [assert_loc_equal ~ctxt expected actual] asserts that the location
      [expected] is equal to the location [actual].  The [ctxt] is passed to all
      internal assertions. *)
end
(** Location Tracking *)
