package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: please input a non-negative integer")
		return
	}

	n, err := strconv.Atoi(os.Args[1])
	if err != nil || n < 0 {
		fmt.Println("Usage: please input a non-negative integer")
		return
	}

	if n == 0 {
		fmt.Println("")
		return
	}

	var result []string

	for n > 0 {
		a, b := 1, 2
		prev := 1

		for b <= n {
			prev = b
			a, b = b, a+b
		}

		result = append(result, strconv.Itoa(prev))
		n -= prev
	}

	fmt.Println(strings.Join(result, ", "))
}