type seed = Seed of Range.t [@@deriving show { with_path = false }]

type mapping = Mapping of Range.t * int
[@@deriving show { with_path = false }]

type table = mapping list [@@deriving show { with_path = false }]
type almanac = seed list * table list [@@deriving show { with_path = false }]
