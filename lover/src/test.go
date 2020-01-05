package main

import (
	"bytes"
	"fmt"
)

type Person struct {
	Name    string
	Age     int
	Numbers []int
}

type Book struct {
	things bytes.Buffer
}

func (p *Person) AddNumber(n int) {
	p.Numbers = append(p.Numbers, n)
}

func main() {
	book := &Book{}
	fmt.Printf("owner: %v", book.things)
	//book.Owner.AddNumber(3)
}
