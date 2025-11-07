{ -- Write to a map
| {key _ next} key val: {key val next} -- update current entry
| {k v next} key val: next key val @! (next: {k v next}) -- update next entry
| {} key val: {key val {}} -- create new entry
}
{ -- Get from a map (runtime error if not present)
| {key val _} key: val
| {_ _ next} key: next key @!
}

(map-set map-get:
	{}
		'a 1 map-set!
		'b 2 map-set!
		'c 3 map-set!
	(map:
		map 'a map-get! print-stack! (1:)
		map 'b map-get! print-stack! (2:)
		map 'c map-get! print-stack! (3:)
		map 'd map-get! -- This should produce an error
	)
)