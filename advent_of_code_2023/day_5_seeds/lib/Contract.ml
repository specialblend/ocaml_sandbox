type seed = Range.t [@@deriving show { with_path = false }]

type mapping = Mapping of Range.t * int
[@@deriving show { with_path = false }]

type table = mapping list list [@@deriving show { with_path = false }]

type almanac = {
  seeds: seed list;
  table: table;
}
[@@deriving show { with_path = false }]
