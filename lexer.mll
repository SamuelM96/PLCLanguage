{
    open Parser
    exception Eof
}

rule token = parse
    [' ' '\t']     { token lexbuf }
    | ['\n' ]      { EOL }
    | eof          { raise Eof }