(**
 * {1 Templates}
 *)

module Value :
  sig
    (**
     * {1 Types}
     *)

    type t
    (** Values *)

    (**
     * {2 Specialized Types}
     *)

    type str = private t
    (** Strings *)

    type num = private t
    (** Numbers *)

    type lst = private t
    (** Lists *)

    type str_lst = private t
    (** Lists of Strings *)

    type num_lst = private t
    (** Lists of Numbers *)

    type intrinsic = private t
    (** Intrinsic Functions *)

    type fn_join = private intrinsic
    (** Join function *)

    type fn_sub = private intrinsic
    (** Substitution function *)

    type fn_transform = private intrinsic
    (** Transform function *)

    type cond = private intrinsic
    (** Conditional Functions *)

    type fn_and = private cond
    (** Logical AND function *)

    type fn_or = private cond
    (** Logical OR function *)

    type pseudo = private t
    (** Pseudo Parameters *)

    (**
     * {1 Strings}
     *)

    val str : string -> str
    (** [str s] constructs the string value representing [s]. *)

    (**
     * {1 Numbers}
     *)

    val int : int -> num
    (** [int i] constructs the number value representing [i]. *)

    val float : float -> num
    (** [float f] constructs the number value representing [f] *)

    (**
     * {1 Lists}
     *)

    val lst : lst
    (** [lst] constructs an empty list *)

    val cons_str : str -> str_lst -> str_lst
    (** [cons_str s lst] conses [s] onto the head of [lst].  Raises
        {!Invalid_argument} if [lst] is not a list of strings. *)

    val is_empty : lst -> bool
    (** [is_empty lst] tests whether [lst] is empty. *)

    val hd_str : lst -> str
    (** [hd_str lst] returns the first element of [lst].  Raises
        {!Invalid_argument} if [lst] is empty or not a list of strings. *)

    val tl : lst -> lst
    (** [tl lst] returns the tail of [lst]. Raises {!Invalid_argument} if [lst]
        is empty or not a list. *)

    (**
     * {1 Intrinsic Functions}
     *)

    val fn_base64 : t -> intrinsic
    (** [fn_base64 value] constructs an intrinsic which Base64 encodes [value]. *)

    val fn_cidr : t -> int -> int -> intrinsic
    (** [fn_cidr block count bits] constructs an intrinsic which divides the
        CIDR block [block] into [count] evenly-sized CIDR blocks spanning [bits]
        bits of mask. *)

    val fn_find_in_map : t -> t -> t -> intrinsic
    (** [fn_find_in_map mapping key1 key2] constructs an intrinsic which
        searches [mapping] for [key1] and then searches the result for [key2]. *)

    val fn_get_att : t -> string -> intrinsic
    (** [fn_get_att res attr] constructs an intrinsic which gets the attribute
        [attr] from the resource [res]. *)

    val fn_get_output : t -> string -> intrinsic
    (** [fn_get_output stack output] is a helper for getting the output named
        [output] of the stack [stack].  Equivalent to
        [fn_get_att stack ("Outputs." ^ output)]. *)

    val fn_get_azs : t -> intrinsic
    (** [fn_get_azs region] constructs an intrinsic which gets the Availability
        zones of [region]. *)

    val fn_import_value : t -> intrinsic
    (** [fn_import_value id] constructs an intrinsic which imports the exported
        value [id]. *)

    val fn_join_all : string -> t list -> intrinsic
    (** [fn_join_all delim values] constructs an intrinsic which joins [values]
        using [delim]. *)

    val fn_select : int -> t -> intrinsic
    (** [fn_select idx value] constructs an intrinsic which selects the
        [idx]'th value from [value]. *)

    val fn_split : string -> t -> intrinsic
    (** [fn_split delim value] constructs an intrinsic which splits [value]
        using [delim]. *)

    val ref : t -> intrinsic
    (** [ref value] constructs an intrinsic which references [value]. *)

    (**
     * {2 Join Function}
     *)

    val fn_join : string -> fn_join
    (** [fn_join delim] constructs an intrinsic which joins values using
        [delim]. *)

    val join_val : t -> fn_join -> fn_join
    (** [join_val value join] prepends [value] to the list of values to be
        joined by [join].  Raises {!Invalid_argument} if [join] is not a join. *)

    (**
     * {2 Substitution Function}
     *)

    val fn_sub : string -> fn_sub
    (** [fn_sub s] constructs a substitution of [s]. *)

    val sub_param : string -> t -> fn_sub -> fn_sub
    (** [sub_param k v sub] adds a parameter named [k] with value [v] to the
        substitution [sub]. *)

    (**
     * {2 Transform Function}
     *)

    val fn_transform : string -> fn_transform
    (** [fn_transform id] constructs a transform named [id]. *)

    val transform_param : string -> t -> fn_transform -> fn_transform
    (** [transform_param k v tr] adds a parameter named [k] with value [v] to
        the transform [tr]. *)

    (**
     * {1 Conditionals}
     *)

    val fn_equals : t -> t -> cond
    (** [fn_equals x y] constructs an intrinsic which checks the equality of [x]
        and [y]. *)

    val fn_if : cond -> t -> t -> cond
    (** [fn_if c t f] constructs an intrinsic which yields to [t] if [c]
        evaluates to true, or yields [f] otherwise. *)

    val fn_not : cond -> cond
    (** [fn_not cond] constructs an intrinsic which returns the logical NOT of
        [cond]. *)

    (**
     * {2 Logical AND}
     *)

    val fn_and : cond -> cond -> fn_and
    (** [fn_and cond cond'] constructs the logical AND of [cond] and [cond']. *)

    val and_cond : cond -> fn_and -> fn_and
    (** [and_cond cond land] adds condition [cond] to the logical AND [land].
        Raises {!Invalid_argument} if [land] is not a logical AND. *)

    (**
     * {2 Logical OR}
     *)

    val fn_or : cond -> cond -> fn_or
    (** [fn_or cond cond'] constructs the logical OR of [cond] and [cond']. *)

    val or_cond : cond -> fn_or -> fn_or
    (** [or_cond cond lor] adds condition [cond] to the logical OR [lor].
        Raises {!Invalid_argument} if [lor] is not a logical OR. *)

    (**
     * {1 Pseudo Parameters}
     *)

    val account_id : pseudo
    (** [account_id] is the AWS Account ID *)

    val notification_arns : pseudo
    (** [notification_arns] is the list of ARNs of SNS topics that notifications
        are sent to. *)

    val partition : pseudo
    (** [partition] is the partition of AWS the template is deployed to. *)

    val region : pseudo
    (** [region] is the region of AWS the template is deployed to. *)

    val stack_id : pseudo
    (** [stack_id] is the ARN of the current stack. *)

    val stack_name : pseudo
    (** [stack_name] is the logical name of the current stack. *)

    val url_suffix : pseudo
    (** [url_suffix] is the suffix of base URLs for services in this partition. *)

    val no_value : pseudo
    (** [no_value] is the special AWS::NoValue value. *)
  end
(*
(**
 * {2 Metadata}
 *)

type wait_time = private
  | WaitSeconds of int
  | WaitForever

type init_command = private {
  command: Value.t;
  env: (string * Value.t) list;
  cwd: Value.t option;
  test: Value.t option;
  ignore_errors: bool option;
  wait_after_completion: wait_time option
}

type file_encoding = private
  | FileEncodingPlain
  | FileEncodingBase64

type init_file = private {
  content: Yojson.Basic.t option;
  source: string option;
  encoding: file_encoding;
  group: string;
  owner: string;
  mode: int;
  authentication: string option;
  context: (string * Value.t) list
}

type packages = private {
  apt: (string * string) list;
  msi: (string * string) list;
  python: (string * string) list;
  rpm: (string * string) list;
  rubygems: (string * string) list;
  yum: (string * string) list
}

type service = private {
  ensure_running: bool option;
  enabled: bool option;
  files: string list;
  sources: string list;
  packages: (string * string list);
  commands: string list
}

type services = private {
  sysvinit: (string * service) list;
  windows: (string * service) list
}

type user = private {
  uid: int option;
  groups: string list;
  home_dir: string option
}

type config = private {
  packages: packages;
  groups: (string * int option) list;
  users: (string * user) list;
  sources: (string * string) list;
  files: (string * init_file) list;
  commands: (string * init_command) list;
  services: services;
}

type config_set = private
  | ConfigSetName of string
  | ConfigName of string

type init = private {
  config_sets: (string * config_set list) list;
  configs: (string * config) list;
}

type interf = private {
  groups: (string * string list) list;
  labels: (string * string) list;
}

type auth = private
  | AuthBasic of string * string * string list
  | AuthS3 of string * string * string option * string list
  | AuthNone

type metadata = private {
  init: init option;
  interf: interf option;
  auth: auth;
  meta: (string * Yojson.Safe.t) list
}

(**
 * {2 Parameters}
 *)

type param_type = private
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

type param = private {
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

(**
 * {2 Mappings}
 *)

type mapping = private (string * Value.t) list
type mappings = private (string * mapping) list

(**
 * {2 Resources}
 *)

type resource = private {
  ty: string;
  metadata: metadata option;
  properties: (string * Yojson.Safe.t) list
}

(**
 * {2 Outputs}
 *)

type output = private {
  description: string option;
  value: Value.t;
  export: string option
}

(**
 * {2 Templates}
 *)

type t = private {
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
