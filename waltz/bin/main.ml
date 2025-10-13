let () =
  let file_name = "my_spec2.txt" in
  
  if not (Sys.file_exists file_name) then begin
    Printf.eprintf "Error: Input file '%s' does not exist\n" file_name;
    exit 1
  end;

  let input_channel = open_in file_name in
  let lexbuf = Lexing.from_channel input_channel in
  
  try
    Printf.printf "Parsing %s...\n" file_name;
    let ast = Waltz.Parser.program Waltz.Lexer.token lexbuf in
    
    Printf.printf "Compiling...\n";
    let modules = Waltz.Ast.compile_to_modules ast in
    
    let write_module (filename, content) =
      let output_channel = open_out filename in
      output_string output_channel (content ^ "\n");
      close_out output_channel;
      Printf.printf "Generated: %s\n" filename;
    in
    
    List.iter write_module modules;
    close_in input_channel;
    Printf.printf "Done.\n"
    
  with
  | Waltz.Parser.Error ->
      Printf.eprintf "Parse error\n";
      close_in input_channel;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      close_in input_channel;
      exit 1
  | e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      close_in input_channel;
      exit 1