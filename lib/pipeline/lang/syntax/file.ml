type top = unit

type file =
  | File of { tops: top list }

let file tops = File { tops }
