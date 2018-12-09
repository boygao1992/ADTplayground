package main

import (
	"fmt"
	"time"
)

func main() {
	terminate := make(chan bool)
	c := make(chan int)
	go ping(c)
	go pong(c, terminate)

	fmt.Println("finished assignment of work")
	<-terminate
}

func ping(c chan<- int) {
	for id := 0; id < 10; id++ {
		time.Sleep(1 * time.Second)
		fmt.Println("... ", id, " snore ...")
		c <- id
	}
}

type Timer struct {
	id     int
	signal <-chan time.Time
}

func pong(c <-chan int, terminate chan<- bool) {
	id := 0
	for {
		gopherID := <-c
		fmt.Println("gopher ", gopherID, " has finished sleeping")

		// terminate if idle for 2 seconds
		id++
		timer := Timer{id, time.After(2 * time.Second)}
		go func() {
			<-timer.signal
			// ignore outdated signals
			if id == timer.id {
				terminate <- true
			}
		}()
	}
}
