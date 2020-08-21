type t = {
  syscall: Syscall.t;
  libc:    Libc.t;
  unwind:  Unwind.t;
  exn:     Exn.t;
  gc:      Gc.t;
  json:    Json.t;
  xml:     Xml.t;
  http:    Http.t
}

let syscall rt = rt.syscall
let libc rt = rt.libc
let unwind rt = rt.unwind
let exn rt = rt.exn
let gc rt = rt.gc
let json rt = rt.json
let xml rt = rt.xml
let http rt = rt.http

let generate md =
  let syscall = Syscall.generate md in
  let libc = Libc.generate md in
  let unwind = Unwind.generate libc md in
  let exn = Exn.generate syscall libc unwind md in
  let gc = Gc.generate libc md in
  let json = Json.generate md in
  let xml = Xml.generate md in
  let http = Http.generate md in
  { syscall = syscall;
    libc    = libc;
    unwind  = unwind;
    exn     = exn;
    gc      = gc;
    json    = json;
    xml     = xml;
    http    = http }
