type mapping = Range.t * int [@@deriving show { with_path = false }]
type table = mapping list [@@deriving show { with_path = false }]
type almanac = Range.t list * table list [@@deriving show { with_path = false }]
