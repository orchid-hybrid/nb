val prepare_note : string -> transaction page
val edit : string -> transaction page
val note : string -> transaction page
val revisionX : string -> int -> transaction page
val history : string -> transaction page

val main : unit -> transaction page
