open Format

let ground fmt = fprintf fmt "_"
let bool fmt b = fprintf fmt "%b" b
let int fmt i = fprintf fmt "%d" i
