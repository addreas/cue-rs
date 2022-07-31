l: [1, 2, 3, 4, 5]
ll:    l[1:2]
lll:   l[:2]
llll:  l[1:]
lllll: l[:]

s:  'Hello \127orld'
ss: 'Hello Worlds'

struct: {a: 1, b: 2}
strutt: {b: 2, a: 1}

i: [1 | "hello"]: "hello"

c: {
	l: !=0
	l: <=2
	l: <3
	t: !="something"
	t: =~"some.*"
	t: !~".*one"
	t: >"somehow"
	t: "somehoww"

	q: "hello" | "world" | _
	q: =~"^(hello|world)$"

	k: <=3
	k: >=1
	k: int
	k: 1 | 2 | 3 | _
}
