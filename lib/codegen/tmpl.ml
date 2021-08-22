open Format

(* Values *)

module Value =
  struct
    let invalid_argument msg =
      let exn = Invalid_argument msg in
      raise exn

    type t =
      | String of string
      | Int of int
      | Float of float
      | List of t * t
      | Empty
      | FnBase64 of t
      | FnCidr of t * int * int
      | FnAnd of cond list
      | FnEquals of t * t
      | FnIf of cond * t * t
      | FnNot of cond
      | FnOr of cond list
      | FnFindInMap of t * t * t
      | FnGetAtt of t * bool * string
      | FnGetAZs of t
      | FnImportValue of t
      | FnJoin of string * t list
      | FnSelect of int * t
      | FnSplit of string * t
      | FnSub of string * (string * t) list
      | FnTransform of string * (string * t) list
      | Ref of t
      | AwsAccountId
      | AwsNotificationArns
      | AwsPartition
      | AwsRegion
      | AwsStackId
      | AwsStackName
      | AwsUrlSuffix
      | AwsNoValue
    and str = t
    and num = t
    and lst = t
    and str_lst = t
    and num_lst = t
    and intrinsic = t
    and fn_join = intrinsic
    and fn_sub = intrinsic
    and fn_transform = intrinsic
    and cond = intrinsic
    and fn_and = cond
    and fn_or = cond
    and pseudo = t

    (* Strings *)

    let str s = String s

    (* Numbers *)

    let int i = Int i
    let float f = Float f

    (* Lists *)

    let not_list _ =
      let msg = sprintf "not a list" in
      invalid_argument msg
    let empty_list _ =
      let msg = sprintf "empty list" in
      invalid_argument msg

    let lst = Empty
    let cons_str x = function
      | Empty -> List (x, Empty)
      | List(String _, _) as lst -> List (x, lst)
      | value -> not_list value
    let is_empty = function
      | Empty -> true
      | List _ -> false
      | value -> not_list value
    let hd_str = function
      | Empty -> empty_list ()
      | List (String _ as hd, _) -> hd
      | value -> not_list value
    let tl = function
      | Empty -> empty_list ()
      | List (_, tl) -> tl
      | value -> not_list value

    (* Intrinsic Functions *)

    let fn_base64 value = FnBase64 value
    let fn_cidr block count bits = FnCidr(block, count, bits)
    let fn_find_in_map map key1 key2 = FnFindInMap(map, key1, key2)
    let fn_get_att value attr = FnGetAtt(value, false, attr)
    let fn_get_output value attr = fn_get_att value ("Outputs." ^ attr)
    let fn_get_azs region = FnGetAZs region
    let fn_import_value name = FnImportValue name
    let fn_join_all delim vals = FnJoin(delim, vals)
    let fn_select idx value = FnSelect(idx, value)
    let fn_split delim value = FnSplit(delim, value)
    let ref value = Ref value

    let not_join _ =
      let msg = sprintf "not a Fn::Join" in
      invalid_argument msg

    let fn_join delim = fn_join_all delim []
    let join_val value = function
      | FnJoin(delim, vals) -> FnJoin(delim, value :: vals)
      | value -> not_join value

    let not_sub _ =
      let msg = sprintf "not a Fn::Sub" in
      invalid_argument msg

    let fn_sub str = FnSub(str, [])
    let sub_param k v = function
      | FnSub(str, params) -> FnSub(str, (k, v) :: params)
      | value -> not_sub value

    let not_transform _ =
      let msg = sprintf "not a Fn::Transform" in
      invalid_argument msg

    let fn_transform id = FnTransform(id, [])
    let transform_param k v = function
      | FnTransform(id, params) -> FnTransform(id, (k, v) :: params)
      | value -> not_transform value

    (* Conditionals *)

    let fn_equals x y = FnEquals(x, y)
    let fn_if c t f = FnIf(c, t, f)
    let fn_not cond = FnNot cond

    let not_and _ =
      let msg = sprintf "not a Fn::And" in
      invalid_argument msg

    let fn_and cond cond' = FnAnd([cond; cond'])
    let and_cond cond = function
      | FnAnd conds -> FnAnd(cond :: conds)
      | value -> not_and value

    let not_or _ =
      let msg = sprintf "not a Fn::Or" in
      invalid_argument msg

    let fn_or cond cond' = FnOr([cond; cond'])
    let or_cond cond = function
      | FnOr conds -> FnOr(cond :: conds)
      | value -> not_or value

    (* Pseudo Parameters *)

    let account_id = AwsAccountId
    let notification_arns = AwsNotificationArns
    let partition = AwsPartition
    let region = AwsRegion
    let stack_id = AwsStackId
    let stack_name = AwsStackName
    let url_suffix = AwsUrlSuffix
    let no_value = AwsNoValue
  end
(*
(* Metadata *)

type wait_time =
  | WaitSeconds of int
  | WaitForever

type init_command = {
  command: Value.t;
  env: (string * Value.t) list;
  cwd: Value.t option;
  test: Value.t option;
  ignore_errors: bool option;
  wait_after_completion: wait_time option
}

type file_encoding =
  | FileEncodingPlain
  | FileEncodingBase64

type init_file = {
  content: Yojson.Basic.t option;
  source: string option;
  encoding: file_encoding;
  group: string;
  owner: string;
  mode: int;
  authentication: string option;
  context: (string * Value.t) list
}

type packages = {
  apt: (string * string) list;
  msi: (string * string) list;
  python: (string * string) list;
  rpm: (string * string) list;
  rubygems: (string * string) list;
  yum: (string * string) list
}

type service = {
  ensure_running: bool option;
  enabled: bool option;
  files: string list;
  sources: string list;
  packages: (string * string list);
  commands: string list
}

type services = {
  sysvinit: (string * service) list;
  windows: (string * service) list
}

type user = {
  uid: int option;
  groups: string list;
  home_dir: string option
}

type config = {
  packages: packages;
  groups: (string * int option) list;
  users: (string * user) list;
  sources: (string * string) list;
  files: (string * init_file) list;
  commands: (string * init_command) list;
  services: services;
}

type config_set =
  | ConfigSetName of string
  | ConfigName of string

type init = {
  config_sets: (string * config_set list) list;
  configs: (string * config) list;
}

type interf = {
  groups: (string * string list) list;
  labels: (string * string) list;
}

type auth =
  | AuthBasic of string * string * string list
  | AuthS3 of string * string * string option * string list
  | AuthNone

type metadata = {
  init: init option;
  interf: interf option;
  auth: auth;
  meta: (string * Yojson.Safe.t) list
}

(* Parameters *)

type param_type =
  | String
  | Number
  | List of param_type
  | AwsEc2AvailabilityZoneName
  | AwsEc2ImageId
  | AwsEc2InstanceId
  | AwsEc2KeyPairName
  | AwsEc2SecurityGroupGroupName
  | AwsEc2SecurityGroupId
  | AwsEc2SubnetId
  | AwsEc2VolumeId
  | AwsEc2VpcId
  | AwsRoute53HostedZoneId
  | AwsSsmParameterName
  | AwsSsmParameterValue of param_type

type param = {
  allowed_pattern: string option;
  allowed_values: Value.t list;
  constraint_description: string option;
  default_value: Value.t option;
  max_length: int option;
  max_value: Value.t option;
  min_length: int option;
  min_value: Value.t option;
  no_echo: bool option;
  param_type: param_type
}

(* Mappings *)

type mapping = (string * Value.t) list
type mappings = (string * mapping) list

(* Resources *)

type resource = {
  ty: string;
  metadata: metadata option;
  properties: (string * Yojson.Safe.t) list
}

(* Outputs *)

type output = {
  description: string option;
  value: Value.t;
  export: string option
}

(* Templates *)

type t = {
  aws_template_format_version: string;
  description: string option;
  metadata: metadata option;
  parameters: (string * param) list;
  mappings: (string * mapping) list;
  conditions: (string * Value.cond) list;
  transform: string list;
  resources: (string * resource) list;
  outputs: (string * output) list
} *)
