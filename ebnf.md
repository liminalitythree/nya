```
program = expr eof
expr = apply
apply = operator ( space+ opapply )*
opapply = primary ( operator primary )*

primary = atom | list | group

list = "[" expr? ("," expr)* "]"
group = "(" expr? ("," expr)* ")"

atom
operator
```