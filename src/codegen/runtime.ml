type t = {
  gc:   Gc.t;
  json: Json.t;
  xml:  Xml.t;
  http: Http.t
}

let generate md =
  let gc   = Gc.generate md in
  let json = Json.generate md in
  let xml  = Xml.generate md in
  let http = Http.generate md in
  { gc   = gc;
    json = json;
    xml  = xml;
    http = http }

let gc rt = rt.gc
let json rt = rt.json
let xml rt = rt.xml
let http rt = rt.http
