open Unix

(*
	ocaml<4.08 compat
	https://github.com/ocaml/ocaml/blob/4.08/otherlibs/unix/unix.ml

	The _pid part of the function names, as well as the pid argument,
	are not needed in the real Unix functions present in 4.08
*)
let open_process_args_full_pid prog args env =
	let (in_read, in_write) = pipe ~cloexec:true () in
	let (out_read, out_write) =
		try pipe ~cloexec:true ()
		with e ->
			close in_read; close in_write;
			raise e in
	let (err_read, err_write) =
		try pipe ~cloexec:true ()
		with e ->
			close in_read; close in_write;
			close out_read; close out_write;
			raise e in
	let inchan = in_channel_of_descr in_read in
	let outchan = out_channel_of_descr out_write in
	let errchan = in_channel_of_descr err_read in
	let pid =
	begin
		try
			create_process_env prog args env out_read in_write err_write
		with e ->
			close out_read; close out_write;
			close in_read; close in_write;
			close err_read; close err_write;
			raise e
	end in
	close out_read;
	close in_write;
	close err_write;
	(inchan, outchan, errchan, pid)

let rec waitpid_non_intr pid =
	try waitpid [] pid
	with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let close_process_full_pid (inchan, outchan, errchan, pid) =
	close_in inchan;
	begin try close_out outchan with Sys_error _ -> () end;
	close_in errchan;
	snd(waitpid_non_intr pid)
(* end ocaml<4.08 compat *)

(* path helpers *)
let as_exe name =
	if Sys.unix then name else name ^ ".exe"

let find_program name =
	let name = as_exe name in
	let pathKey = try Sys.getenv "Path" with Not_found -> "PATH" in
	let path = try Sys.getenv pathKey with Not_found -> "" in
	let pathComponents = Str.split (Str.regexp (if Sys.unix then ":" else ";")) path in
	let sep = if Sys.unix then "/" else "\\" in
	if Sys.file_exists (Sys.getcwd() ^ sep ^ name) then
		Sys.getcwd() ^ sep ^ name
	else
		let indir = List.find (fun dir -> Sys.file_exists (dir ^ sep ^ name)) pathComponents in
		indir ^ sep ^ name
(* end path helpers *)

let command cmd args =
	let cmd = try find_program cmd with Not_found -> cmd in
	let args = Array.of_list (cmd::args) in
	let pin, pout, perr, pid = open_process_args_full_pid cmd args (Unix.environment()) in
	(*ocaml 4.08+: let pin, pout, perr = open_process_args_full cmd args (Unix.environment()) in*)
	let ret = close_process_full_pid (pin,pout,perr,pid) in
	(*ocaml 4.08+: let ret = close_process_full (pin,pout,perr) in*)
	match ret with
	| Unix.WEXITED code -> code
	| _ -> 255
