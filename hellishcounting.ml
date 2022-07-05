#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Thread
open Event

exception HandleStart of string

type communication   = | STOP | CONTINUE | START


let spawn_counter n =
	let channel = Event.new_channel ()
	in
	let rec impl (counter,n) =
		let _ = Event.sync(Event.receive channel)
		in
		if  counter > n then Event.sync(Event.send channel STOP)
		else
			let _ = print_endline (Printf.sprintf "Thread ID: %d: %d" (Thread.id(Thread.self ())) counter)
			in
			let _ = Event.sync(Event.send channel CONTINUE)
			in
			impl ((counter+1),n)
	in
	let _ = Thread.create impl (0,n)
	in
	channel

let run_counters m n =
	let channel = List.init m (fun _ -> spawn_counter n)
	in
	let rec impl channel = match channel with
		| [] -> ()
		| h::t -> let _ = Event.sync(Event.send h START)
					  in
					  match Event.sync(Event.receive h) with
					  | STOP -> impl t
					  | CONTINUE -> impl (t@[h])
					  | _ -> raise (HandleStart "Start can not be reached")
	in
	impl channel
