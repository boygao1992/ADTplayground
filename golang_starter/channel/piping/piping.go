package main

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

func sourceGopher(downstream chan<- string) {
	for _, message := range []string{"hello", "world", "bad day"} {
		time.Sleep(time.Duration(rand.Intn(2)) * time.Second)
		downstream <- message
	}

	downstream <- "end of world"
}

func filterGopher(upstream <-chan string, downstream chan<- string) {
	for {
		message := <-upstream
		if message == "end of world" {
			downstream <- "EOW"
			return
		}
		if !strings.Contains(message, "bad") {
			downstream <- message
		}
	}
}

func printGopher(upstream <-chan string) {
	for {
		message := <-upstream
		if message == "EOW" {
			return
		}
		timestamp := time.Now()
		fmt.Println(timestamp, ": ", message)
	}
}

func main() {
	c0 := make(chan string)
	c1 := make(chan string)
	go sourceGopher(c0)
	go filterGopher(c0, c1)
	printGopher(c1)
}
