open Expressions

(* === UTILITIES === *)

let fresh_counter = ref 0
let fresh_name prefix = 
  incr fresh_counter; 
  prefix ^ string_of_int !fresh_counter

let fresh_flag () = fresh_name "Flag"
let fresh_loop () = fresh_name "mainLoop"

(* === PATTERN AND CONSTRAINT COMPILATION === *)

let rec compile_pat = function
  | PWildcard -> "_"
  | PAtom id -> id
  | PVar id -> String.capitalize_ascii id
  | PList ps -> "{" ^ String.concat ", " (List.map compile_pat ps) ^ "}"

let compile_binop = function
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Eq -> "==" | Neq -> "=/=" | Gt -> ">" | Lt -> "<"
  | Gte -> ">=" | Lte -> "=<" | AndOp -> "andalso" | OrOp -> "orelse"

let rec compile_constraint = function
  | CTrue -> "true"
  | CFalse -> "false" 
  | CBool b -> string_of_bool b
  | CVar v -> String.capitalize_ascii v
  | CNum n -> string_of_int n
  | CBinOp(op, c1, c2) -> 
      "(" ^ compile_constraint c1 ^ " " ^ compile_binop op ^ " " ^ compile_constraint c2 ^ ")"
  | CNot c -> "not (" ^ compile_constraint c ^ ")"

let rec compile_constraint_with_env anchor_vars = function
  | CTrue -> "true"
  | CFalse -> "false" 
  | CBool b -> string_of_bool b
  | CVar v -> 
      let v_lower = String.lowercase_ascii v in
      if List.mem v_lower anchor_vars then
        "maps:get(" ^ v_lower ^ ", Environment)"
      else
        String.capitalize_ascii v 
  | CNum n -> string_of_int n
  | CBinOp(op, c1, c2) -> 
      "(" ^ compile_constraint_with_env anchor_vars c1 ^ " " ^ compile_binop op ^ " " ^ compile_constraint_with_env anchor_vars c2 ^ ")"
  | CNot c -> "not (" ^ compile_constraint_with_env anchor_vars c ^ ")"

(* === STEP REPRESENTATION === *)

type step = {
  src: string;
  dst: string; 
  pattern: pat;
  constraint_val: constraint_value;
}

let signature_to_step = function
  | Signature(Send(src, dst, pat), cval) -> {src; dst; pattern=pat; constraint_val=cval}
  | _ -> failwith "Expected signature"

(* === FORMULA DECOMPOSITION AND ANALYSIS === *)

let rec decompose_to_steps = function
  | Signature(_, _) as sig_form -> [signature_to_step sig_form]
  | Sequence(f1, f2) -> (decompose_to_steps f1) @ (decompose_to_steps f2)
  | _ -> failwith "Cannot decompose formula to steps"

let rec has_immediate_nesting = function
  | Sequence(_f1, f2) -> 
      (match f2 with Theta(_) | Omega(_) -> true | _ -> has_immediate_nesting f2)
  | _ -> false

let rec split_at_nesting = function
  | Sequence(f1, f2) when (match f2 with Theta(_) | Omega(_) -> true | _ -> false) ->
      ([f1], f2)
  | Sequence(f1, f2) ->
      (* Check if f1 is a nested operator - if so, we have a sequence of nested operators *)
      (match f1 with
       | Theta(_) | Omega(_) -> 
           failwith "Sequence of nested operators not supported. Use nested structure instead: THETA(A ; THETA(B ; THETA(C))) not THETA(A ; THETA(B) ; THETA(C))"
       | _ ->
           let (prefix, nested) = split_at_nesting f2 in
           (f1 :: prefix, nested))
  | nested_formula -> ([], nested_formula)

let rec check_modal_consistency outer_modal = function
  | Theta(inner) -> 
      if outer_modal = "OMEGA" then false
      else check_modal_consistency "THETA" inner
  | Omega(inner) -> 
      if outer_modal = "THETA" then false  
      else check_modal_consistency "OMEGA" inner
  | Sequence(f1, f2) -> 
      check_modal_consistency outer_modal f1 && check_modal_consistency outer_modal f2
  | _ -> true

(* === VARIABLE EXTRACTION FOR ENVIRONMENT === *)

let rec extract_pattern_vars = function
  | PVar v -> [String.lowercase_ascii v]
  | PList ps -> List.flatten (List.map extract_pattern_vars ps)
  | _ -> []

let rec extract_pattern_vars_original = function
  | PVar v -> [v]
  | PList ps -> List.flatten (List.map extract_pattern_vars_original ps)
  | _ -> []

let extract_all_vars_from_steps steps =
  List.flatten (List.map (fun step -> extract_pattern_vars step.pattern) steps)

let extract_all_vars_from_steps_original steps =
  List.flatten (List.map (fun step -> extract_pattern_vars_original step.pattern) steps)

let generate_env_creation pattern_vars_original =
  let bindings = List.map (fun v -> 
    String.lowercase_ascii v ^ " => " ^ String.capitalize_ascii v
  ) pattern_vars_original in
  if bindings = [] then "#{}" 
  else "#{" ^ String.concat ", " bindings ^ "}"

(* === SIMPLE MONITOR GENERATION === *)

let generate_simple_theta_monitor steps =
  let flag = fresh_flag () in
  let loop = fresh_loop () in
  
  let rec generate_receives = function
    | [] -> "% Empty sequence"
    | [step] ->
        let constraint_code = compile_constraint step.constraint_val in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        "                ok  % Final step satisfied\n" ^
        "            ; true ->\n" ^
        "                " ^ flag ^ " = true  % Final step violated\n" ^
        "            end\n" ^
        "    end"
    | step :: rest ->
        let constraint_code = compile_constraint step.constraint_val in
        let rest_chain = generate_receives rest in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        "                ok  % Step satisfied\n" ^
        "            ; true ->\n" ^
        "                " ^ flag ^ " = true  % Step violated\n" ^
        "            end,\n" ^
        "            % Always continue with remaining chain\n" ^
        "            " ^ String.concat "\n            " (String.split_on_char '\n' rest_chain) ^ "\n" ^
        "    end"
  in
  
  let receive_chain = generate_receives steps in
  [
    "-module(waltz_monitor).";
    "-export([start/0, stop/0, monitor_loop/0]).";
    "";
    "start() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            Pid = spawn(?MODULE, monitor_loop, []),";
    "            register(waltz_monitor, Pid),";
    "            {ok, Pid};";
    "        Pid ->";
    "            {ok, Pid}";
    "    end.";
    "";
    "stop() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            ok;";
    "        Pid ->";
    "            Pid ! stop,";
    "            unregister(waltz_monitor),";
    "            ok";
    "    end.";
    "";
    "monitor_loop() ->";
    "    receive";
    "        stop ->";
    "            ok;";
    "        _ ->";
    "            " ^ loop ^ "()";
    "    end.";
    "";
    loop ^ "() ->";
    "    " ^ flag ^ " = false,";
    "    " ^ receive_chain ^ ",";
    "    if " ^ flag ^ " ->";
    "        " ^ loop ^ "()  % Chain poisoned, retry";
    "    ; true ->";
    "        satisfied  % Chain satisfied";
    "    end."
  ]

let generate_simple_omega_monitor steps =
  let loop = fresh_loop () in
  
  let rec generate_receives = function
    | [] -> "% Empty sequence"
    | [step] ->
        let constraint_code = compile_constraint step.constraint_val in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        "                " ^ loop ^ "()  % Final step satisfied, loop\n" ^
        "            ; true ->\n" ^
        "                violated  % Final step violated\n" ^
        "            end\n" ^
        "    end"
    | step :: rest ->
        let constraint_code = compile_constraint step.constraint_val in
        let rest_chain = generate_receives rest in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        "                % Step satisfied, continue\n" ^
        "                " ^ String.concat "\n                " (String.split_on_char '\n' rest_chain) ^ "\n" ^
        "            ; true ->\n" ^
        "                violated  % Step violated\n" ^
        "            end\n" ^
        "    end"
  in
  
  let receive_chain = generate_receives steps in
  [
    "-module(waltz_monitor).";
    "-export([start/0, stop/0, monitor_loop/0]).";
    "";
    "start() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            Pid = spawn(?MODULE, monitor_loop, []),";
    "            register(waltz_monitor, Pid),";
    "            {ok, Pid};";
    "        Pid ->";
    "            {ok, Pid}";
    "    end.";
    "";
    "stop() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            ok;";
    "        Pid ->";
    "            Pid ! stop,";
    "            unregister(waltz_monitor),";
    "            ok";
    "    end.";
    "";
    "monitor_loop() ->";
    "    receive";
    "        stop ->";
    "            ok;";
    "        _ ->";
    "            " ^ loop ^ "()";
    "    end.";
    "";
    loop ^ "() ->";
    "    " ^ receive_chain ^ "."
  ]

(* === RECURSIVE SUB-MONITOR GENERATION === *)

(* Recursively compile nested formulas, returning list of (filename, lines) *)
let rec compile_nested_sub_monitors module_name _modal_type parent_anchor_vars nested_formula =
  match nested_formula with
  | Theta(inner) when not (has_immediate_nesting inner) ->
      (* Leaf sub-monitor: simple THETA *)
      let steps = decompose_to_steps inner in
      [(module_name ^ ".erl", generate_sub_monitor module_name "THETA" parent_anchor_vars steps)]
      
  | Omega(inner) when not (has_immediate_nesting inner) ->
      (* Leaf sub-monitor: simple OMEGA *)
      let steps = decompose_to_steps inner in
      [(module_name ^ ".erl", generate_sub_monitor module_name "OMEGA" parent_anchor_vars steps)]
      
  | Theta(inner) when has_immediate_nesting inner ->
      (* Nested THETA: this sub-monitor spawns its own children *)
      let (prefix_formulas, nested_part) = split_at_nesting inner in
      let prefix_steps = List.flatten (List.map decompose_to_steps prefix_formulas) in
      let local_vars = extract_all_vars_from_steps prefix_steps in
      let local_vars_orig = extract_all_vars_from_steps_original prefix_steps in
      let combined_vars = parent_anchor_vars @ local_vars in
      
      (* Generate child sub-monitors recursively *)
      let child_module_name = fresh_name "sub_monitor_" in
      let child_modules = compile_nested_sub_monitors child_module_name "THETA" combined_vars nested_part in
      
      (* Generate this sub-monitor that spawns children *)
      let this_monitor = generate_nested_sub_monitor module_name "THETA" parent_anchor_vars local_vars_orig prefix_steps child_module_name in
      (module_name ^ ".erl", this_monitor) :: child_modules
      
  | Omega(inner) when has_immediate_nesting inner ->
      (* Nested OMEGA: this sub-monitor spawns its own children *)
      let (prefix_formulas, nested_part) = split_at_nesting inner in
      let prefix_steps = List.flatten (List.map decompose_to_steps prefix_formulas) in
      let local_vars = extract_all_vars_from_steps prefix_steps in
      let local_vars_orig = extract_all_vars_from_steps_original prefix_steps in
      let combined_vars = parent_anchor_vars @ local_vars in
      
      let child_module_name = fresh_name "sub_monitor_" in
      let child_modules = compile_nested_sub_monitors child_module_name "OMEGA" combined_vars nested_part in
      
      let this_monitor = generate_nested_sub_monitor module_name "OMEGA" parent_anchor_vars local_vars_orig prefix_steps child_module_name in
      (module_name ^ ".erl", this_monitor) :: child_modules
      
  | _ -> []

(* Generate a simple leaf sub-monitor (no further nesting) *)
and generate_sub_monitor module_name modal_type anchor_vars steps =
  let flag = fresh_flag () in
  let loop = "inner_loop" in
  
  let rec generate_sub_receives = function
    | [] -> "% Empty sequence"
    | [step] ->
        let constraint_code = compile_constraint_with_env anchor_vars step.constraint_val in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        (if modal_type = "THETA" then
        "                ParentPID ! {context_completed, Context, satisfied}\n"
        else
        "                " ^ loop ^ "(Environment, Context, ParentPID)\n") ^
        "            ; true ->\n" ^
        (if modal_type = "THETA" then
        "                " ^ flag ^ " = true\n"
        else
        "                ParentPID ! {context_completed, Context, violated}\n") ^
        "            end;\n" ^
        "        stop ->\n" ^
        "            ok\n" ^
        "    end"
    | step :: rest ->
        let constraint_code = compile_constraint_with_env anchor_vars step.constraint_val in
        let rest_chain = generate_sub_receives rest in
        "receive\n" ^
        "        {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "            Constraint = " ^ constraint_code ^ ",\n" ^
        "            if Constraint ->\n" ^
        "                ok  % Step satisfied\n" ^
        "            ; true ->\n" ^
        (if modal_type = "THETA" then
        "                " ^ flag ^ " = true\n"
        else
        "                ParentPID ! {context_completed, Context, violated}\n") ^
        "            end" ^
        (if modal_type = "THETA" then 
          ",\n            % Always continue with remaining chain\n            " ^ 
          String.concat "\n            " (String.split_on_char '\n' rest_chain) ^ ";\n"
        else
          ";\n") ^
        "        stop ->\n" ^
        "            ok\n" ^
        "    end"
  in
  
  let sub_chain = generate_sub_receives steps in
  let flag_init = if modal_type = "THETA" then "    " ^ flag ^ " = false," else "" in
  let flag_check = if modal_type = "THETA" then
    ",\n    if " ^ flag ^ " ->\n        " ^ loop ^ "(Environment, Context, ParentPID)\n    ; true ->\n        ParentPID ! {context_completed, Context, satisfied}\n    end."
  else "." in
  
  [
    "-module(" ^ module_name ^ ").";
    "-export([start/3]).";
    "";
    "start(Environment, Context, ParentPID) ->";
    "    " ^ loop ^ "(Environment, Context, ParentPID).";
    "";
    loop ^ "(Environment, Context, ParentPID) ->";
    flag_init;
    "    " ^ sub_chain ^ flag_check
  ] |> List.filter (fun s -> s <> "")

(* Generate a sub-monitor that itself spawns children (for deep nesting) *)
and generate_nested_sub_monitor module_name modal_type parent_anchor_vars local_vars_orig steps child_module_name =
  let flag = fresh_flag () in
  let loop = "inner_loop" in
  let anchor_loop = "anchor_loop" in
  let first_step = List.hd steps in
  let is_single = List.length steps = 1 in
  
  let rec generate_remaining = function
    | [] -> ""
    | [step] ->
        let constraint_code = compile_constraint_with_env parent_anchor_vars step.constraint_val in
        let combined_env = generate_env_creation local_vars_orig in
        "receive\n" ^
        "                {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "                    Constraint = " ^ constraint_code ^ ",\n" ^
        "                    if Constraint ->\n" ^
        "                        NewEnv = maps:merge(Environment, " ^ combined_env ^ "),\n" ^
        "                        ChildPID = spawn(" ^ child_module_name ^ ", start, [NewEnv, Context, self()]),\n" ^
        "                        NewChildMap = ChildMap#{Context => ChildPID},\n" ^
        "                        " ^ loop ^ "(Environment, Context, ParentPID, NewChildMap)\n" ^
        "                    ; true ->\n" ^
        (if modal_type = "THETA" then "                        " ^ flag ^ " = true\n" else "                        ParentPID ! {context_completed, Context, violated}\n") ^
        "                    end;\n" ^
        "                stop -> ok\n" ^
        "            end"
    | step :: rest ->
        let constraint_code = compile_constraint_with_env parent_anchor_vars step.constraint_val in
        let rest_chain = generate_remaining rest in
        "receive\n" ^
        "                {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "                    Constraint = " ^ constraint_code ^ ",\n" ^
        "                    if Constraint ->\n" ^
        "                        ok\n" ^
        "                    ; true ->\n" ^
        (if modal_type = "THETA" then "                        " ^ flag ^ " = true\n" else "                        ParentPID ! {context_completed, Context, violated}\n") ^
        "                    end,\n" ^
        "                    " ^ String.concat "\n                    " (String.split_on_char '\n' rest_chain) ^ ";\n" ^
        "                stop -> ok\n" ^
        "            end"
  in
  
  let anchor_body = 
    if is_single then
      let constraint_code = compile_constraint_with_env parent_anchor_vars first_step.constraint_val in
      let combined_env = generate_env_creation local_vars_orig in
      "case {Src, Dst, Msg} of\n" ^
      "        {" ^ first_step.src ^ ", " ^ first_step.dst ^ ", " ^ compile_pat first_step.pattern ^ "} ->\n" ^
      "            Constraint = " ^ constraint_code ^ ",\n" ^
      "            if Constraint ->\n" ^
      "                NewEnv = maps:merge(Environment, " ^ combined_env ^ "),\n" ^
      "                ChildPID = spawn(" ^ child_module_name ^ ", start, [NewEnv, Context, self()]),\n" ^
      "                NewChildMap = ChildMap#{Context => ChildPID},\n" ^
      "                " ^ loop ^ "(Environment, Context, ParentPID, NewChildMap)\n" ^
      "            ; true ->\n" ^
      (if modal_type = "THETA" then "                " ^ flag ^ " = true,\n                " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)\n" else "                ParentPID ! {context_completed, Context, violated}\n") ^
      "            end;\n" ^
      "        _ ->\n" ^
      (if modal_type = "THETA" then "            " ^ flag ^ " = true,\n            " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)\n" else "            " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)\n") ^
      "    end."
    else
      let constraint_code = compile_constraint_with_env parent_anchor_vars first_step.constraint_val in
      let remaining_chain = generate_remaining (List.tl steps) in
      "case {Src, Dst, Msg} of\n" ^
      "        {" ^ first_step.src ^ ", " ^ first_step.dst ^ ", " ^ compile_pat first_step.pattern ^ "} ->\n" ^
      "            Constraint = " ^ constraint_code ^ ",\n" ^
      "            if Constraint ->\n" ^
      "                ok\n" ^
      "            ; true ->\n" ^
      (if modal_type = "THETA" then "                " ^ flag ^ " = true\n" else "                ParentPID ! {context_completed, Context, violated}\n") ^
      "            end,\n" ^
      "            " ^ String.concat "\n            " (String.split_on_char '\n' remaining_chain) ^ ";\n" ^
      "        _ ->\n" ^
      (if modal_type = "THETA" then "            " ^ flag ^ " = true\n" else "            " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)\n") ^
      "    end" ^
      (if modal_type = "THETA" then 
        ",\n    if " ^ flag ^ " -> " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)\n    ; true -> ParentPID ! {context_completed, Context, satisfied}\n    end."
      else ".")
  in
  
  let flag_line = if modal_type = "THETA" then ["    " ^ flag ^ " = false,"] else [] in
  
  [
    "-module(" ^ module_name ^ ").";
    "-export([start/3]).";
    "";
    "start(Environment, Context, ParentPID) ->";
    "    " ^ loop ^ "(Environment, Context, ParentPID, #{}).";
    "";
    loop ^ "(Environment, Context, ParentPID, ChildMap) ->";
    "    receive";
    "        {Src, Dst, Msg, Context} ->";
    "            case maps:get(Context, ChildMap, undefined) of";
    "                undefined ->";
    "                    " ^ anchor_loop ^ "(Src, Dst, Msg, Context, Environment, ParentPID, ChildMap);";
    "                ChildPID ->";
    "                    ChildPID ! {Src, Dst, Msg, Context},";
    "                    " ^ loop ^ "(Environment, Context, ParentPID, ChildMap)";
    "            end;";
    "        {context_completed, Context, Result} ->";
    (if modal_type = "THETA" then
    "            case Result of\n" ^
    "                satisfied -> ParentPID ! {context_completed, Context, satisfied};\n" ^
    "                _ ->\n" ^
    "                    NewChildMap = maps:remove(Context, ChildMap),\n" ^
    "                    " ^ loop ^ "(Environment, Context, ParentPID, NewChildMap)\n" ^
    "            end"
    else
    "            case Result of\n" ^
    "                violated -> ParentPID ! {context_completed, Context, violated};\n" ^
    "                _ ->\n" ^
    "                    NewChildMap = maps:remove(Context, ChildMap),\n" ^
    "                    " ^ loop ^ "(Environment, Context, ParentPID, NewChildMap)\n" ^
    "            end") ^ ";";
    "        stop ->";
    "            maps:fold(fun(_, PID, _) -> PID ! stop end, ok, ChildMap),";
    "            ok";
    "    end.";
    "";
    anchor_loop ^ "(Src, Dst, Msg, Context, Environment, ParentPID, ChildMap) ->";
  ] @ flag_line @ [
    "    " ^ anchor_body
  ] |> List.filter (fun s -> s <> "")

(* === MAIN NESTED MONITOR GENERATION === *)

let generate_main_monitor modal_type anchor_vars_orig steps sub_module_name =
  let flag = fresh_flag () in
  let loop = fresh_loop () in
  let first_step = List.hd steps in
  let is_single = List.length steps = 1 in
  
  let rec generate_remaining = function
    | [] -> ""
    | [step] ->
        let constraint_code = compile_constraint step.constraint_val in
        "receive\n" ^
        "                {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "                    Constraint = " ^ constraint_code ^ ",\n" ^
        "                    if Constraint ->\n" ^
        "                        Environment = " ^ generate_env_creation anchor_vars_orig ^ ",\n" ^
        "                        SubPID = spawn(" ^ sub_module_name ^ ", start, [Environment, Context, self()]),\n" ^
        "                        NewContextMap = ContextMap#{Context => SubPID},\n" ^
        "                        monitor_loop(NewContextMap)\n" ^
        "                    ; true ->\n" ^
        (if modal_type = "THETA" then "                        " ^ flag ^ " = true\n" else "                        violated\n") ^
        "                    end;\n" ^
        "                stop -> ok\n" ^
        "            end"
    | step :: rest ->
        let constraint_code = compile_constraint step.constraint_val in
        let rest_chain = generate_remaining rest in
        "receive\n" ^
        "                {" ^ step.src ^ ", " ^ step.dst ^ ", " ^ compile_pat step.pattern ^ ", Context} ->\n" ^
        "                    Constraint = " ^ constraint_code ^ ",\n" ^
        "                    if Constraint ->\n" ^
        "                        ok\n" ^
        "                    ; true ->\n" ^
        (if modal_type = "THETA" then "                        " ^ flag ^ " = true\n" else "                        violated\n") ^
        "                    end,\n" ^
        "                    " ^ String.concat "\n                    " (String.split_on_char '\n' rest_chain) ^ ";\n" ^
        "                stop -> ok\n" ^
        "            end"
  in
  
  let main_body =
    if is_single then
      let constraint_code = compile_constraint first_step.constraint_val in
      "case {Src, Dst, Msg} of\n" ^
      "        {" ^ first_step.src ^ ", " ^ first_step.dst ^ ", " ^ compile_pat first_step.pattern ^ "} ->\n" ^
      "            Constraint = " ^ constraint_code ^ ",\n" ^
      "            if Constraint ->\n" ^
      "                Environment = " ^ generate_env_creation anchor_vars_orig ^ ",\n" ^
      "                SubPID = spawn(" ^ sub_module_name ^ ", start, [Environment, Context, self()]),\n" ^
      "                NewContextMap = ContextMap#{Context => SubPID},\n" ^
      "                monitor_loop(NewContextMap)\n" ^
      "            ; true ->\n" ^
      (if modal_type = "THETA" then "                " ^ flag ^ " = true,\n                monitor_loop(ContextMap)\n" else "                violated\n") ^
      "            end;\n" ^
      "        _ ->\n" ^
      (if modal_type = "THETA" then "            " ^ flag ^ " = true,\n            monitor_loop(ContextMap)\n" else "            monitor_loop(ContextMap)\n") ^
      "    end."
    else
      let constraint_code = compile_constraint first_step.constraint_val in
      let remaining_chain = generate_remaining (List.tl steps) in
      "case {Src, Dst, Msg} of\n" ^
      "        {" ^ first_step.src ^ ", " ^ first_step.dst ^ ", " ^ compile_pat first_step.pattern ^ "} ->\n" ^
      "            Constraint = " ^ constraint_code ^ ",\n" ^
      "            if Constraint ->\n" ^
      "                ok\n" ^
      "            ; true ->\n" ^
      (if modal_type = "THETA" then "                " ^ flag ^ " = true\n" else "                violated\n") ^
      "            end,\n" ^
      "            " ^ String.concat "\n            " (String.split_on_char '\n' remaining_chain) ^ ";\n" ^
      "        _ ->\n" ^
      (if modal_type = "THETA" then "            " ^ flag ^ " = true\n" else "            monitor_loop(ContextMap)\n") ^
      "    end" ^
      (if modal_type = "THETA" then
        ",\n    if " ^ flag ^ " -> monitor_loop(ContextMap)\n    ; true -> satisfied\n    end."
      else ".")
  in
  
  [
    "-module(waltz_monitor).";
    "-export([start/0, stop/0, monitor_loop/0]).";
    "";
    "start() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            Pid = spawn(?MODULE, monitor_loop, []),";
    "            register(waltz_monitor, Pid),";
    "            {ok, Pid};";
    "        Pid ->";
    "            {ok, Pid}";
    "    end.";
    "";
    "stop() ->";
    "    case whereis(waltz_monitor) of";
    "        undefined ->";
    "            ok;";
    "        Pid ->";
    "            Pid ! stop,";
    "            unregister(waltz_monitor),";
    "            ok";
    "    end.";
    "";
    "monitor_loop() ->";
    "    monitor_loop(#{}).";
    "";
    "monitor_loop(ContextMap) ->";
    "    receive";
    "        stop ->";
    "            maps:fold(fun(_, PID, _) -> PID ! stop end, ok, ContextMap),";
    "            ok;";
    "        {Src, Dst, Msg, Context} ->";
    "            case maps:get(Context, ContextMap, undefined) of";
    "                undefined ->";
    "                    " ^ loop ^ "(Src, Dst, Msg, Context, ContextMap);";
    "                PID ->";
    "                    PID ! {Src, Dst, Msg, Context},";
    "                    monitor_loop(ContextMap)";
    "            end;";
    "        {context_completed, Context, Result} ->";
    "            io:format(\"Context ~p completed: ~p~n\", [Context, Result]),";
    "            NewContextMap = maps:remove(Context, ContextMap),";
    (if modal_type = "THETA" then
    "            case Result of\n" ^
    "                satisfied -> satisfied;\n" ^
    "                _ -> monitor_loop(NewContextMap)\n" ^
    "            end"
    else
    "            case Result of\n" ^
    "                violated -> violated;\n" ^
    "                _ -> monitor_loop(NewContextMap)\n" ^
    "            end");
    "    end.";
    "";
    loop ^ "(Src, Dst, Msg, Context, ContextMap) ->";
    "    " ^ (if modal_type = "THETA" then flag ^ " = false,\n    " else "") ^ main_body
  ]

(* === MAIN COMPILATION === *)

let rec compile_formula = function
  | Theta(inner) when not (has_immediate_nesting inner) ->
      (* Check if inner is directly a modal operator *)
      (match inner with
       | Theta(nested_inner) ->
           (* THETA(THETA(...)) - simplify to THETA(...) *)
           compile_formula (Theta(nested_inner))
       | Omega(_) ->
           (* THETA(OMEGA(...)) - mixed modal operators *)
           [("non_monitorable.erl", ["% Mixed modal operators - not monitorable at runtime"])]
       | _ ->
           (* No nesting, decompose to steps *)
           let steps = decompose_to_steps inner in
           [("waltz_monitor.erl", generate_simple_theta_monitor steps)])
      
  | Omega(inner) when not (has_immediate_nesting inner) ->
      (* Check if inner is directly a modal operator *)
      (match inner with
       | Omega(nested_inner) ->
           (* OMEGA(OMEGA(...)) - simplify to OMEGA(...) *)
           compile_formula (Omega(nested_inner))
       | Theta(_) ->
           (* OMEGA(THETA(...)) - mixed modal operators *)
           [("non_monitorable.erl", ["% Mixed modal operators - not monitorable at runtime"])]
       | _ ->
           (* No nesting, decompose to steps *)
           let steps = decompose_to_steps inner in
           [("waltz_monitor.erl", generate_simple_omega_monitor steps)])
      
  | Theta(inner) when has_immediate_nesting inner ->
      if not (check_modal_consistency "THETA" inner) then
        [("non_monitorable.erl", ["% Mixed modal operators - not monitorable at runtime"])]
      else
        let (prefix_formulas, nested_part) = split_at_nesting inner in
        let prefix_steps = List.flatten (List.map decompose_to_steps prefix_formulas) in
        let anchor_vars = extract_all_vars_from_steps prefix_steps in
        let anchor_vars_orig = extract_all_vars_from_steps_original prefix_steps in
        
        let sub_module_name = "sub_monitor" in
        let sub_modules = compile_nested_sub_monitors sub_module_name "THETA" anchor_vars nested_part in
        let main_monitor = generate_main_monitor "THETA" anchor_vars_orig prefix_steps sub_module_name in
        
        ("waltz_monitor.erl", main_monitor) :: sub_modules
        
  | Omega(inner) when has_immediate_nesting inner ->
      if not (check_modal_consistency "OMEGA" inner) then
        [("non_monitorable.erl", ["% Mixed modal operators - not monitorable at runtime"])]
      else
        let (prefix_formulas, nested_part) = split_at_nesting inner in
        let prefix_steps = List.flatten (List.map decompose_to_steps prefix_formulas) in
        let anchor_vars = extract_all_vars_from_steps prefix_steps in
        let anchor_vars_orig = extract_all_vars_from_steps_original prefix_steps in
        
        let sub_module_name = "sub_monitor" in
        let sub_modules = compile_nested_sub_monitors sub_module_name "OMEGA" anchor_vars nested_part in
        let main_monitor = generate_main_monitor "OMEGA" anchor_vars_orig prefix_steps sub_module_name in
        
        ("waltz_monitor.erl", main_monitor) :: sub_modules
      
  | Sequence(_, _) as seq ->
      let steps = decompose_to_steps seq in
      [("waltz_monitor.erl", generate_simple_theta_monitor steps)]
      
  | Signature(_, _) as sig_form ->
      let steps = [signature_to_step sig_form] in
      [("waltz_monitor.erl", generate_simple_theta_monitor steps)]
      
  | _ ->
      [("error.erl", ["% Unsupported formula type"])]

let compile_to_modules (ast : atl_formula) : (string * string) list =
  try
    let modules = compile_formula ast in
    List.map (fun (name, lines) -> (name, String.concat "\n" lines)) modules
  with 
  | e -> 
    [("compilation_error.erl", Printf.sprintf "%% Compilation failed: %s\n" (Printexc.to_string e))]

let compile (ast : atl_formula) : string =
  try
    let modules = compile_formula ast in
    match modules with
    | (_, main_content) :: others ->
        let other_files = List.map fst others in
        let header_comment = if other_files = [] then "" 
          else "% Additional modules required: " ^ String.concat ", " other_files ^ "\n" in
        header_comment ^ String.concat "\n" main_content
    | [] ->
        "% No modules generated"
  with 
  | e -> 
    Printf.sprintf "%% Compilation failed: %s\n" (Printexc.to_string e)