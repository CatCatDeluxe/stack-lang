-- Linked lists

-- Turns the stack into a linked list
{
	{} {
	| head tail: {head tail} @!
	| :
	}!
}
-- Gets the length of a list
{list:
	0 list {
	| {_ tail}: 1+ tail @!
	| {}:
	}!
}
-- unpacks a linked list
{
| {head tail}: head tail @!
| {}:
}

(list-new list-len list-unpack:
	1 2 3 4 5 list-new! -- create the list
	(list:
		-- (...:) at the end of the line is basically an assertion
		-- if the checks before : do not pass, the program fails because of a
		-- failed match
		list list-len! print-stack! (5:)
		list! print-stack! (1 {2 {3 {4 {5 {}}}}}:)
		list list-unpack! print-stack! (1 2 3 4 5:)
	)
)